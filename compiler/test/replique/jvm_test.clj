(ns replique.jvm-test
  "The JVM back end in dev mode (replique.jvm.dev): source compiled to
  hidden classes and run in this JVM."
  (:require [clojure.test :refer [are deftest is testing]]
            [replique.analyzer :as a]
            [replique.jvm.dev :as dev])
  (:import [replique.lang ArityException ExceptionInfo Printer Var]))

(def ^:private rt (delay (dev/runtime)))

(defn- results
  "What each form of `src` compiles and runs to, in a fresh environment
  unless one is given: the printed value, or the error's code."
  [src & {:keys [cenv]}]
  (let [{rs :results} (dev/load-source @rt (or cenv (a/environment)) src
                                       {:file "t.rpq" :ns 'app.core})]
    (mapv #(if-let [e (:error %)] (:code e) (Printer/print (:value %))) rs)))

(defn- run1 [src & opts] (last (apply results src opts)))

(deftest constants
  (are [src printed] (= printed (run1 src))
    "nil" "nil"
    "true" "true"
    "42" "42"
    "-1.5" "-1.5"
    "\\a" "\\a"
    "\"s\\n\"" "\"s\\n\""
    ":k" ":k"
    ":a/b" ":a/b"
    "'sym" "sym"
    "'a/b" "a/b"
    "1N" "1N"
    "1.5M" "1.5M"
    "1/2" "1/2"
    "String" "#object[java.lang.Class]"))

(deftest special-forms
  (are [src printed] (= printed (run1 src))
    "(if nil 1 2)" "2"
    "(if false 1 2)" "2"
    "(if 0 1 2)" "1"
    "(if false 1)" "nil"
    "(do 1 2 3)" "3"
    "(let [a 1 b (+ a 1)] (* a b))" "2"
    "(let [a 1] (let [a 2] a))" "2"
    "(loop [i 0 acc 0] (if (< i 5) (recur (inc i) (+ acc i)) acc))" "10"
    "(def x 1)" "nil"
    "(case :b :a 1 :b 2 3)" "2"
    "(case 5 (1 2) :low (4 5) :high :other)" ":high"
    "(case 9 1 :a :default)" ":default"))

(deftest functions
  (testing "arities and the rest parameter, nil when empty"
    (is (= ["nil" "1" "3" "#array[3 4]"]
           (results "(defn f ([] 1) ([a b] (+ a b)) ([a b & more] more))
                     (f) (f 1 2) (f 1 2 3 4)"))))
  (is (= "#array[2 3]" (run1 "(defn f ([] 1) ([a & more] more)) (f 1 2 3)")))
  (is (= "nil" (run1 "(defn f [& xs] xs) (f)")))
  (testing "closures, nested"
    (is (= "6" (run1 "(let [a 1] (((fn [b] (fn [c] (+ a b c))) 2) 3))"))))
  (testing "a fn calls itself by its own name"
    (is (= ":done" (run1 "((fn self [n] (if (< n 1) :done (self (dec n)))) 3)"))))
  (testing "and a closure inside it does too"
    (is (= "0" (run1 "((fn self [n] (if (< n 1) n ((fn [] (self (dec n)))))) 3)"))))
  (testing "recur to the top of a fn, variadic or not"
    (is (= "0" (run1 "((fn [n] (if (< 0 n) (recur (dec n)) n)) 5)")))
    (is (= "15" (run1 "(defn sum [acc n] (if (< n 1) acc (recur (+ acc n) (dec n)))) (sum 0 5)")))
    (is (= "#array[2]" (run1 "(defn v [n & r] (if (< n 1) r (recur (dec n) r))) (v 2 2)"))))
  (testing "more than 20 arguments"
    (is (= "#array[21 22]"
           (run1 (str "(defn f [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 & r] r)"
                      "(f " (apply str (interpose " " (range 1 23))) ")")))))
  (testing "a fn value, called through IFn"
    (is (= "3" (run1 "(let [f (fn [a b] (+ a b))] (f 1 2))")))))

(deftest arity-errors-name-the-function
  (let [cenv (a/environment)]
    (results "(defn f [a] a)" :cenv cenv)
    (is (thrown-with-msg? ArityException #"Wrong number of args \(2\) passed to: app.core/f"
                          (results "(f 1 2)" :cenv cenv)))))

(deftest vars
  (testing "a caller sees a redefinition"
    (is (= ["nil" "nil" "1" "nil" "2"]
           (results "(defn f [] 1) (defn g [] (f)) (g) (defn f [] 2) (g)"))))
  (testing "a var linked before it is defined"
    (is (= ["nil" "nil" "nil" "4"]
           (results "(declare w) (defn read-w [] w) (def w 4) (read-w)"))))
  (testing "an unbound var throws when read"
    (is (thrown-with-msg? IllegalStateException #"Var app.core/u is unbound"
                          (results "(declare u) u"))))
  (testing "def inside a fn defines the var when the fn runs"
    (is (= ["nil" ":done" "3"] (results "(defn setup! [] (def z 3) :done) (setup!) z"))))
  (testing "set! changes the root"
    (is (= ["nil" "2" "2"] (results "(def r 1) (set! r 2) r"))))
  (testing "#' is the cell"
    (is (= "(var app.core/x)" (run1 "(def x 1) #'x")))
    (is (instance? Var (do (results "(def x 1)") (Var/find "app.core" "x"))))))

(deftest try-catch-finally
  (are [src printed] (= printed (run1 src))
    "(try (throw (ex-info \"boom\" nil)) (catch :default e (ex-message e)))" "\"boom\""
    "(try 1 (catch Exception _ 2))" "1"
    "(try (throw (ex-info \"a\" 1)) (catch ArithmeticException _ :no) (catch clojure.lang.ExceptionInfo _ :clj) (catch Exception e (ex-data e)))" "1"
    "(try (/ 1 0) (catch ArithmeticException e (ex-message e)))" "\"Divide by zero\""
    "(+ 1 (try 2 (finally 3)))" "3"
    "(let [x 5] (+ 1 (try (throw (ex-info \"x\" nil)) (catch :default _ x))))" "6")
  (testing "finally runs after the body and doesn't change its value"
    (is (= ["nil" "1" ":ran"] (results "(def ran nil) (try 1 (finally (set! ran :ran))) ran"))))
  (testing "finally runs when a catch rethrows"
    (is (= ["nil" ":caught" ":ran"]
           (results "(def ran nil)
                     (try (try (throw (ex-info \"a\" nil)) (catch :default e (throw e)) (finally (set! ran :ran)))
                          (catch :default _ :caught))
                     ran"))))
  (is (thrown? ExceptionInfo (results "(throw (ex-info \"boom\" nil))"))))

(deftest numbers
  (are [src printed] (= printed (run1 src))
    "(+)" "0"
    "(+ 1 2 3)" "6"
    "(+ 1 2.5)" "3.5"
    "(+ 1 1N)" "2N"
    "(- 5)" "-5"
    "(* 2 3 4)" "24"
    "(/ 4 2)" "2"
    "(/ 1 2)" "1/2"
    "(+ 1/2 1/2)" "1N"
    "(/ 1.0 2)" "0.5"
    "(= 1 1)" "true"
    "(= 1 1.0)" "false"
    "(= 1 1N)" "true"
    "(< 1 2 3)" "true"
    "(< 1 3 2)" "false"
    "(not= 1 2)" "true")
  (is (thrown-with-msg? ArithmeticException #"integer overflow"
                        (results "(+ 9223372036854775807 1)"))))

(deftest interop
  (are [src printed] (= printed (run1 src))
    "(.toUpperCase \"abc\")" "\"ABC\""
    "(Math/abs -3)" "3"
    "(Math/max 1 2)" "2"
    "Long/MAX_VALUE" "9223372036854775807"
    "(String. \"x\")" "\"x\""
    "(.length \"abcd\")" "4"
    "(let [s \"abc\"] (.substring s 1))" "\"bc\""
    "(.startsWith ^String (str \"ab\") \"a\")" "true")
  (testing "reflection, in dev mode"
    (is (= "\"BC\"" (run1 "(defn up [s] (.toUpperCase s)) (up \"bc\")")))))

(deftest not-yet
  (are [src code] (= code (run1 src))
    "[1 2]" :jvm/not-implemented
    "'(1 2)" :jvm/not-implemented
    "(letfn [(f [] 1)] (f))" :jvm/not-implemented))
