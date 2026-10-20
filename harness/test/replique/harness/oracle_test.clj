(ns replique.harness.oracle-test
  "The seed suite, on the Clojure oracle. The ClojureScript oracle needs the
  :cljs alias and node, and is run through the command line instead."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [replique.harness.oracle.clojure :as oracle]
            [replique.harness.report :as report]
            [replique.harness.runner :as runner]))

(deftest the-seed-suite-passes-on-clojure
  (let [r (runner/run {:root (io/file "conformance")
                       :paths ["conformance"]
                       :cells [(oracle/cell)]})]
    (is (report/ok? r) (report/text r))
    (is (pos? (get-in (report/summary r) [:oracle-clojure :pass])))))
