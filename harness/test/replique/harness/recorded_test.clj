(ns replique.harness.recorded-test
  "Recording changes nothing a program computes (doc/recording.md §5): the
  suite, run in each dev cell compiled to be recorded, every site chosen,
  gives every check the outcome it has unrecorded. The JS cells are left
  out when node isn't installed."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.test :refer [deftest is]]
            [replique.harness.cell :as cell]
            [replique.harness.js-dev :as js-dev]
            [replique.harness.jvm-dev :as jvm-dev]
            [replique.harness.runner :as runner]))

(def ^:private node?
  (delay (try (zero? (:exit (sh/sh "node" "--version")))
              (catch java.io.IOException _ false))))

(defn- outcomes
  "{[check target] [outcome actual]} for the cells of one kind."
  [results recorded?]
  (into {} (comp (filter #(= recorded? (boolean (re-find #"-recorded$" (name (:cell %))))))
                 (map (juxt (juxt :check :target) (juxt :outcome :actual))))
        results))

(deftest recording-changes-nothing
  (let [cells (cond-> [(jvm-dev/cell) (jvm-dev/cell {:record? true})]
                @node? (into [(js-dev/cell) (js-dev/cell {:record? true})]))]
    (try
      (let [{:keys [results]} (runner/run {:root (io/file "conformance")
                                           :paths ["conformance"]
                                           :cells cells})
            plain (outcomes results false)
            recorded (outcomes results true)]
        (is (pos? (count plain)))
        (is (= (set (keys plain)) (set (keys recorded))))
        (doseq [[k v] plain]
          (is (= v (get recorded k)) (pr-str k))))
      (finally
        (run! cell/-close cells)))))
