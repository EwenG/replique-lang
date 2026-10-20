(ns replique.harness.canon-test
  (:require [clojure.test :refer [deftest is testing]]
            [replique.harness.canon :as canon]))

(defn- jvm [x] (canon/canonical-str x :jvm))
(defn- js [x] (canon/canonical-str x :js))

(deftest numbers-print-with-their-host-type
  (is (= ["1" "1.0" "1/2" "1N" "1.5M"] (map jvm [1 1.0 1/2 1N 1.5M])))
  (testing "in JS an integral double is an integer"
    (is (= ["1" "1" "1.5"] (map js [1 1.0 1.5]))))
  (testing "beyond the safe integers it stays a double"
    (is (= "1.0E17" (js 1e17))))
  (is (= ["##NaN" "##Inf" "##-Inf"]
         (map jvm [Double/NaN Double/POSITIVE_INFINITY Double/NEGATIVE_INFINITY]))))

(deftest collections-are-order-independent
  (is (= (jvm {:a 1 :b #{3 1 2}}) (jvm {:b #{2 3 1} :a 1})))
  (is (= "{:a 1, :b #{1 2 3}}" (jvm {:b #{2 3 1} :a 1})))
  (is (= "[1 (2 3)]" (jvm [1 '(2 3)])))
  (testing "but sequences keep their order"
    (is (not= (jvm [1 2]) (jvm [2 1])))))

(deftest values-without-a-literal
  (is (= "#object[fn]" (jvm (fn []))))
  (is (= "#object[fn]" (jvm (tagged-literal 'object '[fn]))))
  (is (= "(var clojure.core/inc)" (jvm #'inc)))
  (is (= (jvm #'inc) (jvm '(var clojure.core/inc))))
  (is (= "#array[1 2]" (jvm (long-array [1 2]))))
  (is (= "#object[java.lang.Object]" (jvm (Object.)))))

(deftest js-values-are-normalised
  (is (= "[#object[fn] 1]"
         (js (canon/js-value [(tagged-literal 'object '[Function app$core$f]) 1])))))
