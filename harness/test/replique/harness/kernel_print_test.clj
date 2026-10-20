(ns replique.harness.kernel-print-test
  "The harness reads what the kernel's printer writes (doc/conformance.md §3):
  on each target, a value printed there and read back here has the canonical
  form of the value itself."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [replique.harness.canon :as canon]
            [replique.harness.cell :as cell])
  (:import [replique.lang AFn ArityException Keyword Printer Ratio Symbol Var]))

(defn- round-trip [x target]
  (canon/canonical-str (canon/read-printed (Printer/print x) target) target))

(deftest jvm-values-read-back
  (doseq [[x expected]
          [[nil "nil"] [true "true"] [1 "1"] [1.0 "1.0"] [-0.0 "-0.0"] [1.5e-7 "1.5E-7"]
           [##NaN "##NaN"] [##-Inf "##-Inf"]
           [(biginteger 10) "10N"] [1.5M "1.5M"] [(Ratio. (biginteger 1) (biginteger 2)) "1/2"]
           ["a\"b\n\u0001\ud83d" (pr-str "a\"b\n\u0001\ud83d")] [\a "\\a"] [\newline "\\newline"]
           [(Keyword/intern "a" "b") ":a/b"] [(Symbol/of "x") "x"]
           [(Var/intern "kernel.print" "x") "(var kernel.print/x)"]
           [(proxy [AFn] []) "#object[fn]"]
           [(object-array [1 (long-array [2])]) "#array[1 #array[2]]"]
           [(java.util.Date. 0) "#object[java.util.Date]"]]]
    (is (= expected (round-trip x :jvm)) (Printer/print x))))

(defn- node-print
  "Each JS expression printed by runtime.js, or nil without node."
  [exprs]
  (let [url (str (.toURI (io/file "runtime/src/js/replique/runtime.js")))
        src (str "import * as rt from " (pr-str url) ";\n"
                 (str/join "\n" (for [e exprs] (str "console.log(rt.print(" e "));"))))
        r (try (sh/sh "node" "--input-type=module" "-e" src)
               (catch java.io.IOException _ nil))]
    (when r
      (when-not (zero? (:exit r)) (throw (ex-info (:err r) r)))
      (str/split-lines (:out r)))))

(deftest js-values-read-back
  (let [cases [["null" "nil"] ["1" "1"] ["1.5" "1.5"] ["2 ** 60" "1.152921504606847E18"]
               ["1e21" "1.0E21"] ["1.5e-7" "1.5E-7"] ["-0" "0"] ["NaN" "##NaN"]
               ["10n" "10N"] ["'a\\n\"'" (pr-str "a\n\"")]
               ["rt.keyword('a', 'b')" ":a/b"] ["rt.symbol(null, 'x')" "x"]
               ["new rt.Var(rt.namespace('kernel.print'), 'x', 'x')" "(var kernel.print/x)"]
               ["() => 1" "#object[fn]"] ["[1, [null]]" "#array[1 #array[nil]]"]
               ["new Date(0)" "#object[Date]"]]]
    (when-let [out (node-print (map first cases))]
      (doseq [[[e expected] text] (map vector cases out)]
        (testing e
          (is (= expected (canon/canonical-str (canon/read-printed text :js) :js))))))))

(deftest one-value-two-targets
  (testing "an integral number prints the same from both, once canonical for JS"
    (is (= (round-trip 1.0 :js) "1"))))

(deftest arity-errors-are-arity
  (is (= :arity (cell/category (.getName ArityException) "Wrong number of args (1) passed to: f")))
  (is (= :arity (cell/category "ArityException" "Wrong number of args (1) passed to: f"))
      "the JS kernel's"))
