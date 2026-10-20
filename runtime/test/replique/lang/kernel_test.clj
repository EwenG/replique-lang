(ns replique.lang.kernel-test
  "The host kernel (doc/runtime.md §2): the JVM's in Java, through interop,
  and the JS one's agreement with it, through node."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import [java.lang.invoke CallSite MethodHandle MethodHandles MethodType]
           [replique.lang AFn ArityException IFn Keyword Linker Murmur3 Namespace
            Printer Ratio RestFn Symbol Var]))

(def names
  "Names whose hashes both kernels compute: odd and even lengths, non-ASCII,
  a surrogate pair."
  [[nil "a"] [nil "ab"] ["a" "b"] ["replique.core" "fn"] [nil "été"]
   ["ns.with-dash" "name?"] [nil "\ud83d\ude00"] [nil ""]])

;; --- keywords and symbols --------------------------------------------------

(deftest keywords-are-interned
  (is (identical? (Keyword/intern "a" "b") (Keyword/intern "a" "b")))
  (is (identical? (Keyword/intern "b") (Keyword/intern nil "b")))
  (is (not (identical? (Keyword/intern "b") (Keyword/intern "a" "b"))))
  (is (= "a/b" (.-fqn (Keyword/intern "a" "b"))))
  (is (= ":a/b" (str (Keyword/intern "a" "b")))))

(deftest hashes-are-clojures
  (doseq [[ns n] names]
    (testing (pr-str ns n)
      (is (= (hash (symbol ns n)) (.hashCode (Symbol/of ns n))))
      (is (= (hash (keyword ns n)) (.hashCode (Keyword/intern ns n))))))
  (is (= (Murmur3/hashUnencodedChars "abc")
         (clojure.lang.Murmur3/hashUnencodedChars "abc"))))

(deftest symbols-are-values
  (let [s (Symbol/of "a" "b")]
    (is (= s (Symbol/of "a" "b")))
    (is (= s (.withMeta s {:m 1})))
    (is (= {:m 1} (.meta (.withMeta s {:m 1}))))
    (is (not= s (Symbol/of "b")))
    (is (= "a/b" (str s)))))

;; --- the calling convention ------------------------------------------------

(defn- call [^IFn f & args] (.applyTo f (object-array args)))

(deftest arity-errors-name-the-function
  (let [f (proxy [AFn] [] (fnName [] "app.core/f"))]
    (is (thrown-with-msg? ArityException #"Wrong number of args \(2\) passed to: app.core/f"
                          (.invoke ^IFn f 1 2)))
    (is (= 25 (try (apply call f (range 25)) (catch ArityException e (.-actual e)))))))

(deftest apply-dispatches-on-the-count
  (let [f (proxy [AFn] [] (invoke [& args] (vec args)))]
    (is (= [] (call f)))
    (is (= [1 2 3] (call f 1 2 3)))))

(deftest a-variadic-function-gets-every-argument
  (let [f (proxy [RestFn] []
            (fnName [] "app.core/v")
            (requiredArity [] 1)
            (doInvoke [args] [(vec args) (some-> (RestFn/rest args 1) vec)]))]
    (is (= [[1] nil] (.invoke ^IFn f 1)) "no rest arguments is nil")
    (is (= [[1 2 3] [2 3]] (.invoke ^IFn f 1 2 3)))
    (is (= [(vec (range 25)) (vec (range 1 25))] (apply call f (range 25))))
    (is (thrown? ArityException (.invoke ^IFn f)))
    (is (thrown? ArityException (call f)))))

;; --- var cells and linking -------------------------------------------------

(def ^:private lookup (MethodHandles/lookup))

(defn- fresh-ns [] (str (gensym "kernel.test")))

(defn- invoker
  "A call site for (ns/name args...) with `arity` arguments, as the bootstrap
  would make it, and a function that calls it."
  [ns n arity]
  (let [^CallSite cs (Linker/invoke lookup "invoke" (MethodType/genericMethodType (int arity)) ns n)
        h (.dynamicInvoker cs)]
    (fn [& args] (.invokeWithArguments h ^java.util.List (vec args)))))

(defn- reader [ns n]
  (let [^CallSite cs (Linker/value lookup "value" (MethodType/genericMethodType 0) ns n)
        h (.dynamicInvoker cs)]
    (fn [] (.invokeWithArguments h ^java.util.List []))))

(defn- constant-fn [x] (proxy [AFn] [] (invoke [& _] x)))

(deftest a-var-cell
  (let [ns (fresh-ns)
        v (Var/intern ns "x")]
    (is (identical? v (Var/intern ns "x")))
    (is (identical? v (Var/find ns "x")))
    (is (identical? v (.findVar (Namespace/find ns) "x")))
    (is (nil? (Var/find ns "y")))
    (is (not (.isBound v)))
    (is (thrown-with-msg? IllegalStateException #"is unbound" (.deref v)))
    (let [sp (.switchPoint v)]
      (.bindRoot v 1)
      (is (= 1 (.deref v)))
      (is (.hasBeenInvalidated sp) "binding invalidates the SwitchPoint")
      (is (not (.hasBeenInvalidated (.switchPoint v)))))))

(deftest callers-see-a-redefinition
  (let [ns (fresh-ns)
        v (Var/intern ns "f")
        call-f (invoker ns "f" 1)
        read-f (reader ns "f")]
    (.bindRoot v (proxy [AFn] [] (invoke [x] [:one x])))
    (is (= [:one 5] (call-f 5)))
    (is (= [:one 6] (call-f 6)) "linked, and still right")
    (let [f2 (proxy [AFn] [] (invoke [x] [:two x]))]
      (.bindRoot v f2)
      (is (= [:two 5] (call-f 5)))
      (is (identical? f2 (read-f))))
    (.bindRoot v 42)
    (is (= 42 (read-f)))
    (is (thrown-with-msg? ClassCastException #"is not a function" (call-f 1)))))

(deftest a-site-linked-before-the-definition
  (let [ns (fresh-ns)
        call-w (invoker ns "w" 0)
        read-w (reader ns "w")]
    (is (thrown-with-msg? IllegalStateException #"unbound fn" (call-w)))
    (is (thrown-with-msg? IllegalStateException #"is unbound" (read-w)))
    (.bindRoot (Var/intern ns "w") (constant-fn 4))
    (is (= 4 (call-w)))
    (is (= 4 (.invoke ^IFn (read-w))))))

(deftest a-function-with-a-direct-entry
  (let [ns (fresh-ns)
        direct (MethodHandles/dropArguments (MethodHandles/constant Object :direct) 0
                                            ^"[Ljava.lang.Class;" (into-array Class [Object]))
        f (proxy [AFn] []
            (invoke [_] :generic)
            (linkTo [^MethodType t] (when (= 1 (.parameterCount t)) direct)))
        _ (.bindRoot (Var/intern ns "f") f)]
    (is (= :direct ((invoker ns "f" 1) 0)) "the site calls the entry")
    (is (= :generic (.invoke ^IFn f 0)) "a call through the value doesn't")
    (testing "an entry that doesn't fit the site falls back to invoke"
      (let [g (proxy [AFn] [] (invoke [] :generic) (linkTo [_] direct))]
        (.bindRoot (Var/intern ns "g") g)
        (is (= :generic ((invoker ns "g" 0))))))))

(deftest a-site-with-primitive-types
  (let [ns (fresh-ns)
        ^CallSite cs (Linker/invoke lookup "invoke"
                                    (MethodType/methodType Long/TYPE Long/TYPE) ns "inc")
        h (.dynamicInvoker cs)]
    (.bindRoot (Var/intern ns "inc") (proxy [AFn] [] (invoke [x] (inc x))))
    (is (= 2 (.invokeWithArguments h ^java.util.List [1])))))

(deftest many-arguments
  (let [ns (fresh-ns)
        _ (.bindRoot (Var/intern ns "f") (proxy [RestFn] []
                                           (requiredArity [] 0)
                                           (doInvoke [args] (vec args))))
        call-f (invoker ns "f" 22)]
    (is (= (range 22) (apply call-f (range 22))) "past 20, the site calls applyTo")
    (is (= (range 22) (.invoke ^IFn (.deref (Var/find ns "f"))
                               0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
                               (object-array [20 21])))
        "invoke with 20 and the rest in an array")))

(deftest constants
  (is (identical? (Keyword/intern "a" "b") (Linker/keyword lookup "_" Keyword (into-array String ["a" "b"]))))
  (is (identical? (Keyword/intern "b") (Linker/keyword lookup "_" Keyword (into-array String ["b"]))))
  (is (= (Symbol/of "a" "b") (Linker/symbol lookup "_" Symbol (into-array String ["a" "b"]))))
  (let [ns (fresh-ns)]
    (is (identical? (Var/intern ns "x") (Linker/var lookup "_" Var ns "x")))))

;; --- the printer -----------------------------------------------------------

(def printed
  "Values and their text, on the JVM."
  (let [ns (fresh-ns)]
    [[nil "nil"] [true "true"] [false "false"]
     [1 "1"] [(int -3) "-3"] [1.0 "1.0"] [-0.0 "-0.0"] [1.5e-7 "1.5E-7"]
     [##NaN "##NaN"] [##Inf "##Inf"] [##-Inf "##-Inf"] [(float 0.5) "0.5"]
     [(biginteger 10) "10N"] [1.5M "1.5M"]
     [(Ratio. (biginteger 1) (biginteger 2)) "1/2"]
     ["a\"b\\c\n" "\"a\\\"b\\\\c\\n\""] ["\u0001\u007f" "\"\\u0001\\u007f\""]
     ["\ud83d\ude00" "\"\ud83d\ude00\""] ["\ud83d" "\"\\ud83d\""]
     [\a "\\a"] [\newline "\\newline"] [\space "\\space"] [(char 1) "\\u0001"]
     [(Keyword/intern "a" "b") ":a/b"] [(Symbol/of "x") "x"]
     [(Var/intern ns "x'") (str "(var " ns "/x')")]
     [(constant-fn 1) "#object[fn]"]
     [(object-array [1 "a" (long-array [2 3])]) "#array[1 \"a\" #array[2 3]]"]
     [(java.util.Date. 0) "#object[java.util.Date]"]]))

(deftest the-printer
  (doseq [[x s] printed]
    (is (= s (Printer/print x)) (pr-str x))))

;; --- the JS kernel ---------------------------------------------------------

(defn- node
  "Run `script` as an ES module that has imported runtime.js as rt; its
  output, or nil when node isn't installed."
  [script]
  (let [url (str (.toURI (io/file "runtime/src/js/replique/runtime.js")))
        src (str "import * as rt from " (pr-str url) ";\n" script)
        r (try (sh/sh "node" "--input-type=module" "-e" src)
               (catch java.io.IOException _ nil))]
    (when r
      (when-not (zero? (:exit r)) (throw (ex-info (:err r) r)))
      (:out r))))

(defn- js-str
  "`s` as a JS string literal, every character but printable ASCII escaped,
  so a lone surrogate survives the command line."
  [s]
  (if (nil? s)
    "null"
    (str "\"" (apply str (for [c s]
                            (if (and (<= 0x20 (int c) 0x7e) (not (#{\" \\} c)))
                              c
                              (format "\\u%04x" (int c)))))
         "\"")))

(deftest js-hashes-agree
  (when-let [out (node (str/join "\n" (for [[ns n] names]
                                        (str "console.log(rt.symbol(" (js-str ns) ", " (js-str n) ").hash,"
                                             " rt.keyword(" (js-str ns) ", " (js-str n) ").hash);"))))]
    (is (= (for [[ns n] names] (str (hash (symbol ns n)) " " (hash (keyword ns n))))
           (str/split-lines out)))))

(deftest js-prints-like-the-jvm
  (testing "strings escape alike"
    (let [strs ["a\"b\\c\n\t\r\b\f" "\u0001\u007f" "\ud83d\ude00" "\ud83d" "\udc00x" "été"]]
      (when-let [out (node (str/join "\n" (for [s strs] (str "console.log(rt.print(" (js-str s) "));"))))]
        (is (= (map #(Printer/print %) strs) (str/split-lines out)))))))

(deftest node-tests
  (let [r (try (sh/sh "node" "--test" "runtime/test/js/runtime.test.mjs")
               (catch java.io.IOException _ nil))]
    (when r
      (is (zero? (:exit r)) (str (:out r) (:err r))))))
