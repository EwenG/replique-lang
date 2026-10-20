(ns replique.harness.runner
  "The matrix: every case file, in every cell that should run it, compared with
  its expectations (doc/conformance.md §4, §5)."
  (:require [replique.harness.canon :as canon]
            [replique.harness.cases :as cases]
            [replique.harness.cell :as cell]))

(defn applies?
  "Whether `cell` runs case `c`, or why not."
  [{:keys [target mode oracle?]} c]
  (cond
    (not ((:targets c) target)) [false (str "not for " (name target))]
    (not ((:modes c) mode)) [false (str "not in " (name mode) " mode")]
    (and oracle? (not (:clojure? c))) [false "not marked ^:clojure"]
    :else [true nil]))

(defn- resolve-case
  "A case with its reader conditionals resolved for `target`."
  [c target]
  (update c :steps
          (fn [steps]
            (mapv (fn [s]
                    (if (= :form (:op s))
                      (update s :form cases/resolve-conditionals target)
                      (-> s
                          (update :expected cases/resolve-conditionals target)
                          (update :expr cases/resolve-conditionals target))))
                  steps))))

(defn- describe
  "A step result as text, for the report."
  [{:keys [result value category data message code]} target]
  (case result
    :value (canon/canonical-str value target)
    :thrown (str "threw " category
                 (when (some? data) (str " " (canon/canonical-str data target)))
                 (when message (str ": " message)))
    :compile-error (str "compile error " code (when message (str ": " message)))
    nil "nothing"))

(defn judge
  "The outcome of one check, given what the cell returned for it. Returns
  {:outcome :pass/:fail/:error :expected text :actual text}."
  [{:keys [op expected]} actual target]
  (let [shown (describe actual target)
        mk (fn [outcome expected-text]
             {:outcome outcome :expected expected-text :actual shown})]
    (case op
      :expect
      (let [want (canon/canonical-str expected target)]
        (case (:result actual)
          :value (mk (if (= want shown) :pass :fail) want)
          (mk :error want)))

      :expect-throws
      (let [[cat data] (if (vector? expected) expected [expected nil])
            want (str "threw " cat (when (some? data) (str " " (canon/canonical-str data target))))]
        (if (and (= :thrown (:result actual))
                 (= cat (:category actual))
                 (or (nil? data)
                     (= (canon/canonical-str data target)
                        (canon/canonical-str (:data actual) target))))
          (mk :pass want)
          (mk (if (= :compile-error (:result actual)) :error :fail) want)))

      :expect-error
      (let [want (str "compile error " expected)]
        (if (and (= :compile-error (:result actual)) (= expected (:code actual)))
          (mk :pass want)
          (mk :fail want))))))

(defn- step-failed? [r]
  (not= :value (:result r)))

(defn- case-results
  "One result entry per check of case `c` in `cell-info`, from the cell's step
  results (possibly fewer than the steps, when one failed to run)."
  [cell-info c step-results]
  (let [target (:target cell-info)
        base (fn [s] {:check (:id s) :case (:id c) :name (:name c)
                      :file (:file c) :span (:span s) :cell (:id cell-info)
                      :target target :mode (:mode cell-info)})]
    (loop [steps (:steps c) rs step-results aborted nil out []]
      (if-let [s (first steps)]
        (let [r (first rs)]
          (cond
            aborted
            (recur (rest steps) (rest rs) aborted
                   (if (= :form (:op s))
                     out
                     (conj out (assoc (base s) :outcome :error
                                      :actual (str "not run: " aborted)))))

            (= :form (:op s))
            (if (or (nil? r) (step-failed? r))
              (let [why (str "the form at line " (:line (:span s)) " "
                             (if r (describe r target) "did not run"))]
                (recur (rest steps) (rest rs) why
                       (conj out (assoc (base (assoc s :id (str (:id c) "/" (:line (:span s)))))
                                        :outcome :error :actual why))))
              (recur (rest steps) (rest rs) nil out))

            :else
            (recur (rest steps) (rest rs) nil
                   (conj out (merge (base s)
                                    (if r
                                      (judge s r target)
                                      {:outcome :error :actual "the cell returned no result"}))))))
        out))))

(defn- skipped [cell-info c why]
  (for [s (cases/checks c)]
    {:check (:id s) :case (:id c) :name (:name c) :file (:file c) :span (:span s)
     :cell (:id cell-info) :target (:target cell-info) :mode (:mode cell-info)
     :outcome :skipped :actual why}))

(defn run-file
  "Results for one parsed case file in one cell."
  [the-cell file]
  (let [info (cell/-info the-cell)
        {run true skip false} (group-by #(first (applies? info %)) (:cases file))
        resolved (mapv #(resolve-case % (:target info)) run)
        returned (if (seq resolved)
                   (try
                     (cell/-run-file the-cell
                                     (update file :ns-form cases/resolve-conditionals (:target info))
                                     resolved)
                     (catch Throwable t
                       ;; the cell itself broke: every case of the file is an
                       ;; error, and the message says so
                       (let [r {:result :thrown :category :host
                                :message (str "the cell failed: " (or (ex-message t) (class t)))}]
                         (into {} (map (fn [c] [(:id c) [r]])) resolved))))
                   {})]
    (concat
     (mapcat (fn [c]
               (if-let [rs (get returned (:id c))]
                 (case-results info c rs)
                 (skipped info c "the cell does not run it")))
             resolved)
     (mapcat #(skipped info % (second (applies? info %))) skip))))

(defn run
  "Run every case file under `paths` in every cell of `cells`. Returns the
  report data of doc/conformance.md §5."
  [{:keys [root paths cells]}]
  (let [files (mapv #(cases/read-file root %) (cases/case-files paths))
        results (vec (for [c cells, f files, r (run-file c f)] r))]
    {:cells (mapv cell/-info cells)
     :files (mapv :rel files)
     :results results}))
