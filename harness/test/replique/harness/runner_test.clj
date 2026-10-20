(ns replique.harness.runner-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [replique.harness.cell :as cell]
            [replique.harness.report :as report]
            [replique.harness.runner :as runner])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- fake-cell
  "A cell that answers each step from `answer`, a function of the step's
  expression (or form) and the cell's info."
  [info answer]
  (reify cell/Cell
    (-info [_] info)
    (-run-file [_ _ cases]
      (into {}
            (map (fn [c]
                   [(:id c)
                    (loop [steps (:steps c) out []]
                      (if-let [s (first steps)]
                        (let [r (answer (or (:expr s) (:form s)) info)
                              out (conj out r)]
                          (if (and (= :form (:op s)) (not= :value (:result r)))
                            out
                            (recur (rest steps) out)))
                        out))]))
            cases))
    (-close [_])))

(defn- value [v] {:result :value :value v})

(defn- suite
  "A suite of one file holding `src`, run in `cells`."
  [src cells]
  (let [root (.toFile (Files/createTempDirectory "harness-test" (make-array FileAttribute 0)))
        f (io/file root "x.rpq")]
    (spit f src)
    (runner/run {:root root :paths [root] :cells cells})))

(def ^:private jvm-dev {:id :jvm-dev :target :jvm :mode :dev})
(def ^:private jvm-adv {:id :jvm-advanced :target :jvm :mode :advanced})

(defn- outcomes [report]
  (mapv (juxt :check :cell :outcome) (:results report)))

(deftest judging-each-kind-of-check
  (let [r {:result :thrown :category :ex-info :data {:a 1} :message "m"}
        e {:result :compile-error :code :replique/x}]
    (is (= :pass (:outcome (runner/judge {:op :expect :expected 1} (value 1) :jvm))))
    (is (= :fail (:outcome (runner/judge {:op :expect :expected 1} (value 2) :jvm))))
    (is (= :error (:outcome (runner/judge {:op :expect :expected 1} r :jvm))))
    (is (= :pass (:outcome (runner/judge {:op :expect :expected 1.0} (value 1) :js))))
    (is (= :pass (:outcome (runner/judge {:op :expect-throws :expected :ex-info} r :jvm))))
    (is (= :pass (:outcome (runner/judge {:op :expect-throws :expected [:ex-info {:a 1}]} r :jvm))))
    (is (= :fail (:outcome (runner/judge {:op :expect-throws :expected [:ex-info {:a 2}]} r :jvm))))
    (is (= :fail (:outcome (runner/judge {:op :expect-throws :expected :arity} r :jvm))))
    (is (= :pass (:outcome (runner/judge {:op :expect-error :expected :replique/x} e :jvm))))
    (is (= :fail (:outcome (runner/judge {:op :expect-error :expected :replique/y} e :jvm))))))

(deftest cases-run-only-where-they-apply
  (let [report (suite "(ns x)\n^{:modes #{:dev}} (expect 1 1)\n^{:targets #{:js}} (expect 1 1)\n(expect 1 1)"
                      [(fake-cell jvm-dev (fn [_ _] (value 1)))
                       (fake-cell jvm-adv (fn [_ _] (value 1)))
                       (fake-cell {:id :oracle :target :jvm :mode :dev :oracle? true}
                                  (fn [_ _] (value 1)))])]
    (is (= {:jvm-dev {:pass 2 :fail 0 :error 0 :skipped 1}
            :jvm-advanced {:pass 1 :fail 0 :error 0 :skipped 2}
            :oracle {:pass 0 :fail 0 :error 0 :skipped 3}}
           (report/summary report)))
    (is (= #{"not for jvm" "not in advanced mode" "not marked ^:clojure"}
           (set (keep #(when (= :skipped (:outcome %)) (:actual %)) (:results report)))))))

(deftest a-failing-form-aborts-its-scenario
  (let [report (suite "(ns x)\n(scenario \"s\"\n  (setup)\n  (expect 1 1)\n  (expect 2 2))"
                      [(fake-cell jvm-dev (fn [form _]
                                            (if (= '(setup) form)
                                              {:result :thrown :category :host :message "no"}
                                              (value 1))))])]
    (is (= [["x.rpq:2/3" :jvm-dev :error] ["x.rpq:2/4" :jvm-dev :error] ["x.rpq:2/5" :jvm-dev :error]]
           (outcomes report)))
    (is (str/starts-with? (:actual (second (:results report))) "not run: the form at line 3"))))

(deftest reader-conditionals-are-resolved-before-the-cell-sees-them
  (let [seen (atom [])
        report (suite "(ns x)\n(expect #?(:jvm 1 :js 2) (f #?(:jvm :a :js :b)))"
                      [(fake-cell jvm-dev (fn [form _] (swap! seen conj form) (value 1)))])]
    (is (= ['(f :a)] @seen))
    (is (report/ok? report))))

(deftest a-broken-cell-is-an-error-not-a-crash
  (let [broken (reify cell/Cell
                 (-info [_] jvm-dev)
                 (-run-file [_ _ _] (throw (RuntimeException. "boom")))
                 (-close [_]))
        report (suite "(ns x)\n(expect 1 1)" [broken])]
    (is (= [["x.rpq:2" :jvm-dev :error]] (outcomes report)))
    (is (str/includes? (:actual (first (:results report))) "the cell failed: boom"))))

(deftest mode-divergences-come-first
  (let [report (suite "(ns x)\n(expect 1 (f))\n(expect 1 (g))"
                      [(fake-cell jvm-dev (fn [_ _] (value 1)))
                       (fake-cell jvm-adv (fn [form _] (value (if (= '(f) form) 2 1))))])]
    (is (= [["x.rpq:2" :jvm-advanced :fail]]
           (mapv (juxt :check :cell :outcome) (report/mode-divergences report))))
    (is (str/includes? (report/text report) "Dev and advanced mode disagree"))
    (is (not (report/ok? report)))))
