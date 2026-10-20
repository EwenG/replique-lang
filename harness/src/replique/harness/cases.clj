(ns replique.harness.cases
  "Case files to case records (doc/conformance.md §2).

  A case file is read with Clojure's reader, reader conditionals preserved, so
  that one file can be resolved once per target. The case forms (expect,
  expect-throws, expect-error, scenario) are recognised here by name: they are
  not macros of any language.

  A case is a map:

    :id       \"special-forms/if.rpq:3\", the file relative to the suite root
              and the line
    :file     the absolute path, for editors
    :ns       the namespace the file declares
    :name     the scenario's name, or the expression's text
    :span     {:line :column :end-line :end-column}, 1-based, the end
              exclusive
    :modes    a subset of #{:dev :advanced}
    :targets  a subset of #{:jvm :js}
    :clojure? whether the oracle cells run it (§6)
    :steps    a vector of steps, in order

  A step is either a form to run, {:op :form :form f :span s}, or a check:

    {:op :expect        :expected v   :expr e :id ... :span ...}
    {:op :expect-throws :expected cat :expr e :id ... :span ...}
    {:op :expect-error  :expected code :expr e :id ... :span ...}

  A standalone expect is a case with a single check, so cells and the runner
  only ever deal with one shape."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [clojure.lang LineNumberingPushbackReader ReaderConditional]
           [java.io File]))

(def all-modes #{:dev :advanced})
(def all-targets #{:jvm :js})

(def ^:private check-ops '#{expect expect-throws expect-error})
(def ^:private case-meta-keys #{:modes :targets :clojure :line :column})

(defn- case-error
  "A case file that can't be read is an error with its position, not a skipped
  case: a typo in a case form would otherwise silently test nothing."
  [file span msg]
  (ex-info (str file ":" (:line span) ":" (:column span) ": " msg)
           {:replique.harness/error :case-file :file file :span span}))

;; --- reading ---------------------------------------------------------------

(defn- read-all
  "Every top-level form of `file`, each with its span. Reader conditionals are
  preserved, and unknown tags are kept as tagged literals, so the expected
  values of a case can use any tag."
  [^File file]
  (with-open [rdr (LineNumberingPushbackReader. (io/reader file))]
    (binding [*default-data-reader-fn* tagged-literal
              *read-eval* false]
      (let [eof (Object.)]
        (loop [forms []]
          (let [form (read {:read-cond :preserve :eof eof} rdr)]
            (if (identical? eof form)
              forms
              (let [{:keys [line column]} (meta form)]
                (recur (conj forms
                             {:form form
                              :span {:line line :column column
                                     :end-line (.getLineNumber rdr)
                                     :end-column (.getColumnNumber rdr)}}))))))))))

(defn- form-span
  "The span of a nested form, as far as the reader recorded it: lists and
  vectors carry a start position, nothing carries an end."
  [form outer]
  (let [{:keys [line column]} (meta form)]
    (if line {:line line :column column} outer)))

;; --- reader conditionals ---------------------------------------------------

(defn resolve-conditionals
  "`form` with every reader conditional replaced by the branch for `target`
  (:jvm or :js), falling back to :default. A conditional with no matching
  branch reads as nothing, as in Clojure. Metadata is kept, so positions
  survive. A form that resolves to nothing is nil."
  [form target]
  (letfn [(branch [^ReaderConditional rc]
            (let [pairs (partition 2 (.-form rc))]
              (some (fn [[k v]] (when (#{target :default} k) [v])) pairs)))
          (expand [x]
            ;; a sequence of the forms x stands for: one, or several for #?@,
            ;; or none
            (if (instance? ReaderConditional x)
              (if-let [[v] (branch x)]
                (if (.-splicing x) (mapcat expand v) (expand v))
                [])
              [(walk x)]))
          (walk [x]
            (let [m (meta x)
                  out (cond
                        (instance? clojure.lang.TaggedLiteral x)
                        (tagged-literal (:tag x) (walk (:form x)))
                        (map-entry? x) (vec (mapcat expand x))
                        (seq? x) (apply list (mapcat expand x))
                        (vector? x) (vec (mapcat expand x))
                        (map? x) (into (empty x)
                                       (map vec)
                                       (partition 2 (mapcat expand (apply concat x))))
                        (set? x) (into (empty x) (mapcat expand x))
                        :else x)]
              (if (and m (instance? clojure.lang.IObj out))
                (with-meta out m)
                out)))]
    ;; the form itself may be a conditional; one splicing at the top has no
    ;; sequence to splice into, so it is read as its first form
    (first (expand form))))

;; --- case forms ------------------------------------------------------------

(defn- head [form]
  (when (and (seq? form) (symbol? (first form)))
    (first form)))

(defn- text [form]
  (let [s (binding [*print-meta* false] (pr-str form))]
    (if (> (count s) 60) (str (subs s 0 57) "...") s)))

(defn- check-step
  "A check form as a step. `span` is the check's own span."
  [file form span]
  (let [op (head form)]
    (when-not (= 3 (count form))
      (throw (case-error file span (str "(" op " expected expr) takes two arguments"))))
    (let [[_ expected expr] form]
      (when (and (= 'expect-error op) (not (keyword? expected)))
        (throw (case-error file span "expect-error takes a diagnostic code, a keyword")))
      (when (and (= 'expect-throws op)
                 (not (or (keyword? expected)
                          (and (vector? expected) (keyword? (first expected))))))
        (throw (case-error file span "expect-throws takes a category, or [category data]")))
      {:op (keyword op) :expected expected :expr expr :span span})))

(defn- restriction
  "The :modes or :targets of a case, checked against the allowed values."
  [file span m k allowed]
  (let [v (get m k allowed)]
    (when-not (and (set? v) (seq v) (every? allowed v))
      (throw (case-error file span (str k " must be a non-empty subset of " allowed))))
    v))

(defn- parse-case
  [file rel ns-sym {:keys [form span]}]
  (let [op (head form)
        m (meta form)
        unknown (remove case-meta-keys (keys m))]
    (when (seq unknown)
      (throw (case-error file span (str "unknown case options " (vec unknown)))))
    (let [base {:id (str rel ":" (:line span))
                :file (.getPath ^File file)
                :ns ns-sym
                :span span
                :modes (restriction file span m :modes all-modes)
                :targets (restriction file span m :targets all-targets)
                :clojure? (boolean (:clojure m))}]
      (cond
        (check-ops op)
        (assoc base
               :name (text (last form))
               :steps [(check-step file form span)])

        (= 'scenario op)
        (let [[_ name & body] form]
          (when-not (string? name)
            (throw (case-error file span "(scenario \"name\" form...) needs a name")))
          (when (empty? body)
            (throw (case-error file span "a scenario needs at least one form")))
          (assoc base
                 :name name
                 :steps (mapv (fn [f]
                                (let [s (form-span f span)]
                                  (if (check-ops (head f))
                                    (check-step file f s)
                                    {:op :form :form f :span s})))
                              body)))

        :else
        (throw (case-error file span
                           (str "expected expect, expect-throws, expect-error or scenario, got "
                                (text form))))))))

(defn- number-checks
  "Give each check an id: the case's id for a standalone check, the case's id
  and the check's line inside a scenario."
  [c]
  (update c :steps
          (fn [steps]
            (mapv (fn [s]
                    (if (= :form (:op s))
                      s
                      (assoc s :id (if (= 1 (count steps))
                                     (:id c)
                                     (str (:id c) "/" (:line (:span s)))))))
                  steps))))

(defn read-file
  "The cases of one case file. `root` is the suite's root directory, which ids
  are relative to."
  [^File root ^File file]
  (let [rel (-> (.relativize (.toPath (.getCanonicalFile root))
                             (.toPath (.getCanonicalFile file)))
                str
                (str/replace File/separator "/"))
        [ns-form & cases] (read-all file)]
    (when-not (= 'ns (head (:form ns-form)))
      (throw (case-error file (:span ns-form) "a case file starts with an ns form")))
    (let [ns-sym (second (:form ns-form))]
      {:file (.getPath file)
       :rel rel
       :ns ns-sym
       :ns-form (:form ns-form)
       :cases (mapv #(number-checks (parse-case file rel ns-sym %)) cases)})))

(defn case-files
  "Every .rpq file under `paths` (files or directories), sorted, so runs are
  reproducible."
  [paths]
  (->> paths
       (map io/file)
       (mapcat file-seq)
       (filter #(and (.isFile ^File %) (str/ends-with? (.getName ^File %) ".rpq")))
       (sort-by #(.getPath ^File %))))

(defn checks
  "The check steps of a case."
  [c]
  (remove #(= :form (:op %)) (:steps c)))
