(ns replique.js-test
  "The JS back end in dev mode (replique.js.dev): source compiled to JS and
  run in Node. Skipped when node isn't installed."
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [are deftest is testing]]
            [replique.analyzer :as a]
            [replique.analyzer.bootstrap :as bootstrap]
            [replique.js.dev :as dev]
            [replique.js.emit :as emit]
            [replique.js.names :as names]))

(def ^:private node?
  (delay (try (zero? (:exit (sh/sh "node" "--version")))
              (catch java.io.IOException _ false))))

(def ^:private rt (delay (dev/runtime)))

(defn- outcome [r]
  (cond (:error r) (:code (:error r))
        (:thrown r) [:thrown (:class (:thrown r)) (:message (:thrown r))]
        :else (:value r)))

(defn- results
  "What each form of `src` compiles and runs to, in a fresh environment
  unless one is given: the printed value, the error's code, or
  [:thrown class message]."
  [src & {:keys [cenv]}]
  (mapv outcome (:results (dev/load-source @rt (or cenv (a/environment)) src
                                           {:file "t.rpq" :ns 'app.core}))))

(defn- run1 [src & opts] (last (apply results src opts)))

(defmacro ^:private with-node [& body]
  `(if @node? (do ~@body) (println "node isn't installed: skipping" ~(str *ns*))))

(defn- js-of
  "The JS a form compiles to, in namespace app.core."
  [src]
  (let [cenv (a/environment)
        [r] (:units (a/analyze-source cenv src {:file "t.rpq" :ns 'app.core :target :js}))]
    (emit/form-js 'app.core (:node r))))

(deftest names-are-injective
  (let [cs (map str "-_$?!<>=*+/.:'abc")
        names (for [a cs b cs c cs] (str a b c))
        munged (map names/munge names)]
    (is (= (count (distinct names)) (count (distinct munged)))))
  (is (= "foo_bar" (names/munge "foo-bar")))
  (is (= "foo$US$bar" (names/munge "foo_bar")))
  (is (= "app$core$ns" (names/ns-alias 'app.core)))
  (is (= "x__3" (names/local-name 'x 3)))
  (is (= "v__4" (names/local-name 'v__11__auto__ 4)) "an auto-gensym's suffix is dropped"))

(deftest no-function-wrappers
  (testing "an if, a let and a try in an argument are statements and a temporary"
    (let [js (js-of "(str (if (str) 1 2) (let [a 1] a) (try 3 (finally 4)))")]
      (is (not (str/includes? js "function ()")) js)
      (is (= 1 (count (re-seq #"function" js))) js))))

(deftest kernel-names-are-the-compilers
  (with-node
    (let [script (str "import * as rt from " (pr-str (str (.toURI (java.io.File. "runtime/src/js/replique/runtime.js"))))
                      "; rt.install(); console.log(Object.keys(rt.namespace('replique.core')).join(' '));")
          defined (set (str/split (str/trim (:out (sh/sh "node" "--input-type=module" "-e" script))) #" "))
          ;; what the JVM kernel installs (replique.lang.Core) is what JS installs
          installed '[+ * - / inc dec = not= < > <= >= not nil? some? identity str ex-info ex-data
                      ex-message ex-cause instance? keyword symbol deref count pr-str prn print println]]
      (is (every? (set bootstrap/kernel-fns) installed))
      (is (= (set (map names/munge installed)) defined)))))

(deftest constants
  (with-node
    (are [src printed] (= printed (run1 src))
      "nil" "nil"
      "true" "true"
      "42" "42"
      "-1.5" "-1.5"
      "1.0" "1"
      "\"s\\n\"" "\"s\\n\""
      ":k" ":k"
      ":a/b" ":a/b"
      "'sym" "sym"
      "'a/b" "a/b"
      "1N" "1N"
      "##NaN" "##NaN"
      "##-Inf" "##-Inf")
    (is (= "0.5" (run1 "1/2")) "a ratio reads as a double in JS")))

(deftest special-forms
  (with-node
    (are [src printed] (= printed (run1 src))
      "(if nil 1 2)" "2"
      "(if 0 1 2)" "1"
      "(if \"\" 1 2)" "1"
      "(if false 1)" "nil"
      "(do 1 2 3)" "3"
      "(let [a 1 b (+ a 1)] (* a b))" "2"
      "(let [a 1] (let [a 2] a))" "2"
      "(loop [i 0 acc 0] (if (< i 5) (recur (inc i) (+ acc i)) acc))" "10"
      "(loop [a 1 b 2 n 0] (if (< n 1) (recur b a (inc n)) (- a b)))" "1"   ; recur rebinds at once
      "(def x 1)" "nil"
      "(case :b :a 1 :b 2 3)" "2"
      "(case 5 (1 2) :low (4 5) :high :other)" ":high"
      "(case 1.0 1 :one :other)" ":one"
      "(case \\a \"a\" :str :other)" ":str"
      "(case 'y x :x y :y :none)" ":y"
      "(case :x (1 \"a\") :one :x :kw y :sym :none)" ":kw"
      "(case 'y (1 \"a\") :one :x :kw y :sym :none)" ":sym"
      "(case 9 1 :a :default)" ":default"))
  (with-node
    (is (= [:thrown "Error" "No matching clause: 3"] (run1 "(case 3 1 :a 2 :b)")))))

(deftest functions
  (with-node
    (testing "arities and the rest parameter, nil when empty"
      (is (= ["nil" "1" "3" "#array[3 4]"]
             (results "(defn f ([] 1) ([a b] (+ a b)) ([a b & more] more))
                       (f) (f 1 2) (f 1 2 3 4)"))))
    (is (= "nil" (run1 "(defn f [& xs] xs) (f)")))
    (testing "closures, nested"
      (is (= "6" (run1 "(let [a 1] (((fn [b] (fn [c] (+ a b c))) 2) 3))"))))
    (testing "a fn calls itself by its own name, from a closure too"
      (is (= ":done" (run1 "((fn self [n] (if (< n 1) :done (self (dec n)))) 3)")))
      (is (= "0" (run1 "((fn self [n] (if (< n 1) n ((fn [] (self (dec n)))))) 3)"))))
    (testing "recur to the top of a fn, variadic or not"
      (is (= "15" (run1 "(defn sum [acc n] (if (< n 1) acc (recur (+ acc n) (dec n)))) (sum 0 5)")))
      (is (= "#array[2]" (run1 "(defn v [n & r] (if (< n 1) r (recur (dec n) r))) (v 2 2)")))
      (is (= "3" (run1 "(defn m ([] 0) ([n] (if (< n 3) (recur (inc n)) n))) (m 0)"))))
    (testing "a closure made in a loop keeps its iteration's values"
      (is (= "1" (run1 "((loop [i 0 g nil] (if (< i 3) (recur (inc i) (if (= i 1) (fn [] i) g)) g)))"))))
    (testing "a throw in an argument"
      (is (= [:thrown "ExceptionInfo" "x"] (run1 "(+ 1 (throw (ex-info \"x\" nil)))"))))))

(deftest arity-errors-name-the-function
  (with-node
    (is (= [:thrown "ArityException" "Wrong number of args (2) passed to: app.core/f"]
           (run1 "(defn f [a] a) (f 1 2)")))
    (is (= [:thrown "ArityException" "Wrong number of args (1) passed to: app.core/g"]
           (run1 "(defn g ([] 0) ([a b] 2)) (g 1)")))
    (is (= [:thrown "ArityException" "Wrong number of args (0) passed to: app.core/h"]
           (run1 "(defn h [a & r] a) (h)")))))

(deftest vars
  (with-node
    (testing "a caller sees a redefinition"
      (is (= ["nil" "nil" "1" "nil" "2"]
             (results "(defn f [] 1) (defn g [] (f)) (g) (defn f [] 2) (g)"))))
    (testing "a var linked before it is defined"
      (is (= ["nil" "nil" "nil" "4"]
             (results "(declare w) (defn read-w [] w) (def w 4) (read-w)"))))
    (testing "an unbound var throws when read"
      (is (= [:thrown "Error" "Var app.core/u is unbound"] (run1 "(declare u) u"))))
    (testing "names that munge alike in ClojureScript are two vars"
      (is (= "3" (run1 "(def foo-bar 1) (def foo_bar 2) (+ foo-bar foo_bar)"))))
    (testing "def inside a fn defines the var when the fn runs"
      (is (= ["nil" ":done" "3"] (results "(defn setup! [] (def z 3) :done) (setup!) z"))))
    (testing "set! changes the root"
      (is (= ["nil" "2" "2"] (results "(def r 1) (set! r 2) r"))))
    (testing "#' is the var"
      (is (= "(var app.core/x)" (run1 "(def x 1) #'x")))
      (is (= "1" (run1 "(def x 1) (deref #'x)"))))))

(deftest try-catch-finally
  (with-node
    (are [src printed] (= printed (run1 src))
      "(try (throw (ex-info \"boom\" nil)) (catch :default e (ex-message e)))" "\"boom\""
      "(try 1 (catch js/Error _ 2))" "1"
      "(try (throw (ex-info \"a\" 1)) (catch js/TypeError _ :no) (catch js/Error e (ex-data e)))" "1"
      "(try (.foo nil) (catch js/TypeError _ :type-error))" ":type-error"
      "(+ 1 (try 2 (finally 3)))" "3"
      "(let [x 5] (+ 1 (try (throw (ex-info \"x\" nil)) (catch :default _ x))))" "6")
    (testing "nothing matching rethrows"
      (is (= [:thrown "ExceptionInfo" "a"] (run1 "(try (throw (ex-info \"a\" nil)) (catch js/TypeError _ :no))"))))
    (testing "finally runs after the body and when a catch rethrows"
      (is (= ["nil" "1" ":ran"] (results "(def ran nil) (try 1 (finally (set! ran :ran))) ran")))
      (is (= ["nil" ":caught" ":ran"]
             (results "(def ran nil)
                       (try (try (throw (ex-info \"a\" nil)) (catch :default e (throw e)) (finally (set! ran :ran)))
                            (catch :default _ :caught))
                       ran"))))))

(deftest numbers
  (with-node
    (are [src printed] (= printed (run1 src))
      "(+)" "0"
      "(+ 1 2 3)" "6"
      "(+ 1 2.5)" "3.5"
      "(+ 1N 2N)" "3N"
      "(- 5)" "-5"
      "(* 2 3 4)" "24"
      "(/ 1 2)" "0.5"
      "(/ 4)" "0.25"
      "(= 1 1.0)" "true"
      "(= 1 1N)" "false"
      "(< 1 2 3)" "true"
      "(< 1 3 2)" "false"
      "(not= 1 2)" "true"
      "(str 1.0 nil :a)" "\"1:a\"")
    (is (= [:thrown "TypeError" "\"a\" is not a number"] (run1 "(+ 1 \"a\")"))
        "arithmetic on a non-number throws")))

(deftest interop
  (with-node
    (are [src printed] (= printed (run1 src))
      "(.toUpperCase \"abc\")" "\"ABC\""
      "(js/Math.abs -3)" "3"
      "(. js/Math max 1 2)" "2"
      "(.-length \"abcd\")" "4"
      "(.getTime (js/Date. 0))" "0"
      "(let [s \"abc\"] (.substring s 1))" "\"bc\""
      "(js* \"~{} + ~{}\" 1 2)" "3"
      "#js [1 2]" "#array[1 2]")))

(deftest not-yet
  (with-node
    (are [src code] (= code (run1 src))
      "[1 2]" :js/not-implemented
      "'(1 2)" :js/not-implemented
      "(letfn [(f [] 1)] (f))" :js/not-implemented)))
