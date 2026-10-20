(ns replique.harness.report
  "The report (doc/conformance.md §5): data first, text from the data."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(def ^:private outcomes [:pass :fail :error :skipped])

(defn summary
  "{cell-id {:pass n :fail n :error n :skipped n}}"
  [{:keys [cells results]}]
  (let [counts (frequencies (map (juxt :cell :outcome) results))]
    (into (sorted-map)
          (for [{:keys [id]} cells]
            [id (into {} (for [o outcomes] [o (get counts [id o] 0)]))]))))

(defn mode-divergences
  "Checks that pass in one mode and not in the other, on the same target: the
  bugs doc/compilation.md §3.5 exists to prevent. Oracle cells take no part."
  [{:keys [cells results]}]
  (let [oracle (set (keep #(when (:oracle? %) (:id %)) cells))
        by-key (group-by (juxt :check :target)
                         (remove #(or (oracle (:cell %)) (= :skipped (:outcome %))) results))]
    (for [[_ rs] by-key
          :let [modes (group-by :mode rs)]
          :when (and (:dev modes) (:advanced modes)
                     (not= (= :pass (:outcome (first (:dev modes))))
                           (= :pass (:outcome (first (:advanced modes))))))
          r rs
          :when (not= :pass (:outcome r))]
      r)))

(defn failures [{:keys [results]}]
  (filter #(#{:fail :error} (:outcome %)) results))

(defn ok? [report]
  (empty? (failures report)))

(defn- location [{:keys [file span]}]
  (str file ":" (:line span) ":" (:column span)))

(defn- failure-text [r]
  (str (location r) "  " (name (:cell r)) "  " (name (:outcome r)) "\n"
       "  " (:name r) "\n"
       (when (:expected r) (str "  expected: " (:expected r) "\n"))
       "  actual:   " (:actual r) "\n"))

(defn text
  "The report as text: the summary table, then mode divergences, then the
  other failures."
  [report]
  (let [s (summary report)
        width (apply max 4 (map (comp count name) (keys s)))
        pad (fn [x n] (format (str "%-" n "s") x))
        divergent (mode-divergences report)
        divergent-ids (set (map (juxt :check :cell) divergent))
        others (remove #(divergent-ids [(:check %) (:cell %)]) (failures report))]
    (str/join
     (concat
      [(pad "cell" width) "  pass  fail  error  skipped\n"]
      (for [[id c] s]
        (format (str "%-" width "s  %4d  %4d  %5d  %7d\n")
                (name id) (:pass c) (:fail c) (:error c) (:skipped c)))
      (when (seq divergent)
        (cons "\nDev and advanced mode disagree:\n\n" (map failure-text divergent)))
      (when (seq others)
        (cons "\nFailures:\n\n" (map failure-text others)))
      [(if (ok? report) "\nAll checks passed.\n" "")]))))

(defn write-edn!
  "Write the report data to `path`, for replique.el and CI."
  [report path]
  (let [f (io/file path)]
    (io/make-parents f)
    (with-open [w (io/writer f)]
      (binding [*out* w *print-namespace-maps* false]
        (pp/pprint (assoc report :summary (summary report)))))
    (.getPath f)))
