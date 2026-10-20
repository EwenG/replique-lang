(ns replique.harness.analysis-test
  "Every form of the conformance suite analyses, on both targets: the cases
  the Replique cells will run are within what the analyzer accepts."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [replique.analyzer :as a]
            [replique.analyzer.host :as host]
            [replique.harness.cases :as cases]))

(defn- errors
  "The errors of analysing the cases of `file` for `target`, in order, each
  scenario's forms one after the other as a cell would run them. A check
  that expects a compile error is skipped: its error belongs to a pass that
  isn't written yet."
  [root file target]
  (let [{:keys [ns ns-form cases]} (cases/read-file root file)
        cenv (a/environment)
        h (if (= :js target) (host/js) (host/jvm))
        analyze (fn [form]
                  (let [r (a/analyze-top cenv (cases/resolve-conditionals form target)
                                         {:ns ns :host h})]
                    (filter #(= :error (:severity %)) (:diagnostics r))))]
    (concat
     (analyze ns-form)
     (for [c cases
           :when (contains? (:targets c) target)
           step (:steps c)
           :when (not= :expect-error (:op step))
           e (analyze (if (= :form (:op step)) (:form step) (:expr step)))]
       (assoc e :case (:id c))))))

(deftest the-suite-analyses
  (let [root (io/file "conformance")]
    (doseq [f (cases/case-files [root])
            target [:jvm :js]]
      (testing (str (.getPath f) " for " target)
        (is (empty? (errors root f target)))))))
