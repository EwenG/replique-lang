(ns replique.reader-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [are deftest is testing]]
            [clojure.walk :as walk]
            [replique.reader :as r]
            [replique.reader.cst :as cst]
            [replique.reader.forms :as forms]))

(defn- read1 [src & {:as opts}]
  (r/read-one src (merge {:ns 'app.core} opts)))

(defn- codes [src & {:as opts}]
  (mapv :code (:diagnostics (r/read-source src (merge {:ns 'app.core} opts)))))

(def ^:private tricky
  "Sources that exercise every node kind, including broken ones."
  ["" " , \n" "(a [1 2.5] {:k \"s\"}) ; c\n#{x}" "'q `(a ~b ~@c) @d #'e"
   "#?(:jvm 1 :js 2) #?@(:jvm [3])" "^:m ^{:a 1} #inst \"2020\" \\a \\newline"
   "#_ z #_#_ a b ##Inf #:a{:b 1} #::{:c 2} #(+ % %&)" "#!shebang\n#\"re\\\"\""
   "(open [also {and" ")]}" "\"unterminated" "#= (+ 1 2) #< x" "1x :: :a: #?x #"])

(deftest the-cst-is-lossless
  (doseq [src tricky]
    (is (= src (apply str (map cst/text (:nodes (r/parse src))))) src))
  (testing "on real code"
    (doseq [f ["../../clojure/replique-clj/src/clj/clojure/core.clj"
               "../../clojure/replique-cljs/src/clj/cljs/core.cljs"]
            :when (.exists (io/file f))]
      (let [src (slurp f)]
        (is (= src (apply str (map cst/text (:nodes (r/parse src))))) f)))))

(deftest every-node-has-a-span
  (let [p (r/parse "(a\n  [1 \"s\"])")
        [lst] (:nodes p)
        [_ _ vect] (:children lst)]
    (is (= {:line 1 :column 1 :end-line 2 :end-column 11} (cst/span p lst)))
    (is (= {:line 2 :column 3 :end-line 2 :end-column 10} (cst/span p vect)))
    (is (= {:line 2 :column 6 :end-line 2 :end-column 9}
           (cst/span p (last (cst/forms vect)))))))

(deftest forms-carry-their-spans
  (let [form (read1 "(f 1\n   x :k)" :file "a.rpq")
        span (fn [line col end-line end-col]
               {:line line :column col :end-line end-line :end-column end-col :file "a.rpq"})]
    (is (= (span 1 1 2 9) (select-keys (meta form) [:line :column :end-line :end-column :file])))
    (testing "symbols carry their own"
      (is (= (span 2 4 2 5) (select-keys (meta (nth form 2)) [:line :column :end-line :end-column :file]))))
    (testing "constants are recorded by the collection that holds them"
      (is (= (span 1 4 1 5) (r/element-span form 1)))
      (is (= (span 2 6 2 8) (r/element-span form 3)))
      (is (nil? (r/element-span form 2)) "a symbol has its own"))
    (testing "in maps and sets"
      (let [m (read1 "{:a 1, b \"s\"}")]
        (is (= [2 5] (map :column (r/element-span m :a))))
        (is (= 10 (:column (second (r/element-span m 'b))))))
      (is (= 5 (:column (r/element-span (read1 "#{x :y}") :y)))))))

(deftest atoms
  (are [src v] (= v (read1 src))
    "nil" nil "true" true "false" false
    "42" 42 "-7" -7 "0x1F" 31 "017" 15 "2r101" 5 "36rZ" 35 "1N" 1N
    "9223372036854775808" 9223372036854775808N
    "1.5" 1.5 "1e3" 1000.0 "1.5M" 1.5M "1/2" 1/2 "4/2" 2 "-1/3" -1/3
    "\"a\\tb\\u0041\\101\\\\\\\"\"" "a\tbAA\\\""
    "\\a" \a "\\newline" \newline "\\u0041" \A "\\o101" \A "\\(" \(
    ":k" :k ":a/b" :a/b "::x" :app.core/x
    "sym" 'sym "a/b" 'a/b "/" '/ "a//" 'a//)
  (is (Double/isNaN (read1 "##NaN")))
  (is (= Double/POSITIVE_INFINITY (read1 "##Inf")))
  (is (= "a+b" (str (read1 "#\"a+b\""))))
  (is (= "a\\\"" (str (read1 "#\"a\\\"\""))) "a regex keeps its escapes"))

(deftest numbers-follow-the-target
  (testing "JS numbers are doubles"
    (is (= 0.5 (read1 "1/2" :target :js)))
    (is (= 1N (read1 "1N" :target :js)) "1N is a BigInt")
    (is (= [:reader/bigdecimal-in-js] (codes "1M" :target :js)))
    (is (= [:reader/imprecise-integer] (codes "9007199254740993" :target :js)))
    (is (= [] (codes "9007199254740991" :target :js))))
  (testing "duplicate keys are compared as the target compares them"
    (is (= [] (codes "{1 :a 1.0 :b}")))
    (is (= [:reader/duplicate-key] (codes "{1 :a 1.0 :b}" :target :js)))))

(deftest collections
  (is (= '(a [b {:c #{d}}]) (read1 "(a [b {:c #{d}}])")))
  (is (list? (read1 "(a)")))
  (is (vector? (read1 "[a]")))
  (testing "small maps keep their order"
    (is (= [:z :a :m] (keys (read1 "{:z 1 :a 2 :m 3}")))))
  (is (= [:reader/duplicate-key] (codes "{:a 1 :a 2}")))
  (is (= [:reader/duplicate-key] (codes "#{1 1}")))
  (is (= [:reader/map-odd] (codes "{:a}")))
  (testing "namespaced maps"
    (is (= {:a/x 1 :y 2 'a/z 3} (read1 "#:a{:x 1 :_/y 2 z 3}")))
    (is (= {:app.core/x 1} (read1 "#::{:x 1}")))
    (is (= {:other.ns/x 1} (read1 "#::o{:x 1}" :aliases {'o 'other.ns})))))

(deftest prefixes
  (are [src v] (= v (read1 src))
    "'x" '(quote x)
    "@a" '(replique.core/deref a)
    "#'a" '(var a)
    "#_ a b" 'b
    "'#_ a b" '(quote b))
  (testing "metadata"
    (let [x (read1 "^:a ^String ^{:b 1} [x]")]
      (is (= {:a true :tag 'String :b 1}
             (select-keys (meta x) [:a :tag :b]))))
    (is (= '[long] (:param-tags (meta (read1 "^[long] Math/abs")))))
    (is (= [:reader/invalid-meta] (codes "^:a 1")))))

(deftest reader-conditionals-select-per-target
  (are [src target v] (= v (read1 src :target target))
    "#?(:jvm 1 :js 2)" :jvm 1
    "#?(:jvm 1 :js 2)" :js 2
    "#?(:js 1 :default 3)" :jvm 3
    "[1 #?@(:jvm [2 3]) 4]" :jvm [1 2 3 4]
    "[1 #?@(:jvm [2 3]) 4]" :js [1 4])
  (testing "an unselected branch is never converted"
    (is (= 1 (read1 "#?(:jvm 1 :js #js {:a 1M})"))))
  (testing "no branch is no form"
    (is (= [] (:forms (r/read-source "#?(:js 1)")))))
  (is (= [:reader/invalid-reader-cond] (codes "#?@(:jvm [1])")))
  (is (= [:reader/invalid-reader-cond] (codes "#?(:jvm)"))))

(deftest syntax-quote
  (is (= '(replique.core/seq (replique.core/concat (replique.core/list (quote app.core/a))
                                                   (replique.core/list b)
                                                   c
                                                   (replique.core/list (quote if))))
         (read1 "`(a ~b ~@c if)")))
  (testing "gensyms are consistent inside one syntax-quote, and differ between two"
    (let [[_ [_ [_ [_ a]] [_ [_ b]]]] (read1 "`(x# x#)")
          [_ [_ [_ [_ c]]]] (read1 "`(x#)")]
      (is (= a b))
      (is (re-matches #"x__\d+__auto__" (name a)))
      (is (= c a) "each read starts its own counter...")
      (let [counter (atom 0)
            g #(-> (read1 "`(x#)" :gensym-counter counter) second second second second)]
        (is (not= (g) (g)) "...unless the caller shares one"))))
  (is (= '(quote other.ns/f) (read1 "`o/f" :aliases {'o 'other.ns})))
  (is (= '(quote java.util.Date) (read1 "`java.util.Date")))
  (is (= '(quote app.core/Foo.) (read1 "`Foo.")))
  (is (= :k (read1 "`:k")))
  (is (= [:reader/splice-outside-list] (codes "`~@x")))
  (testing "a symbol's metadata is kept"
    (is (= 'replique.core/with-meta (first (read1 "`^:once fn*")))))
  (testing "the core namespace is an option"
    (is (= 'clojure.core/seq (first (read1 "`(a)" :core-ns 'clojure.core))))))

(deftest fn-literals
  (let [[fn* params body] (read1 "#(+ % %3 %&)")]
    (is (= 'fn* fn*))
    (is (= 5 (count params)))
    (is (= '& (nth params 3)))
    (is (= [(nth params 0) (nth params 2) (nth params 4)] (rest body)))
    (is (= '(fn* [p1__1#] (p1__1# p1__1#)) (read1 "#(% %1)")) "% and %1 are one parameter"))
  (is (= [:reader/nested-fn] (codes "#(#(%))"))))

(deftest tagged-literals
  (is (= (java.time.Instant/parse "2020-01-02T03:04:05Z") (read1 "#inst \"2020-01-02T03:04:05Z\"")))
  (is (= (java.time.Instant/parse "2020-01-01T00:00:00Z") (read1 "#inst \"2020\"")))
  (is (= #uuid "00000000-0000-0000-0000-000000000001"
         (read1 "#uuid \"00000000-0000-0000-0000-000000000001\"")))
  (is (= (tagged-literal 'js {:a 1}) (read1 "#js {:a 1}")))
  (is (= [:app/point 1 2] (read1 "#app/point [1 2]" :tags {'app/point #(into [:app/point] %)})))
  (is (= [:reader/unknown-tag] (codes "#app/point [1 2]")))
  (is (= [:reader/invalid-tagged] (codes "#inst \"nope\"")))
  (is (= [:reader/invalid-tagged] (codes "#app/x 1" :tags {'app/x (fn [_] (throw (Exception. "no")))}))))

(deftest errors-are-diagnostics-and-reading-goes-on
  (let [{:keys [forms diagnostics]} (r/read-source "(a) ) (b \"x\\q\") [c] #=(d) e")]
    (is (= '[(a) [c] e] (map :form forms)))
    (is (= [:reader/unmatched-delimiter :reader/read-eval :reader/invalid-escape]
           (map :code diagnostics)))
    (is (= {:line 1 :column 5 :end-line 1 :end-column 6}
           (:span (first diagnostics)))))
  (are [src code] (= [code] (codes src))
    "1x" :reader/invalid-number
    ":a:" :reader/invalid-token
    "#?x" :reader/invalid-reader-cond
    "#%" :reader/unknown-dispatch
    "\\bogus" :reader/invalid-char
    "\"\\u12\"" :reader/invalid-escape
    "#\"(\"" :reader/invalid-regex
    "::o/x" :reader/unknown-alias
    "(a))" :reader/unmatched-delimiter))

(deftest incomplete-input
  (are [src] (:incomplete? (r/read-source src))
    "(a" "[1 {2" "\"abc" "'" "\\" "#")
  (are [src] (not (:incomplete? (r/read-source src)))
    "(a)" "(a))" "(a 1x")
  (testing "::keywords need a namespace"
    (is (= [:reader/no-namespace] (mapv :code (:diagnostics (r/read-source "::x")))))))

(defn- normalize
  "Gensyms and regexes made comparable between two readers."
  [form]
  (walk/postwalk
   (fn [x]
     (cond
       (and (symbol? x) (re-find #"__\d+(__auto__)?#?$|^p\d+__\d+#$|^rest__\d+#$" (name x))) 'GENSYM
       (instance? java.util.regex.Pattern x) [:regex (str x)]
       :else x))
   form))

(defn- java-lang-resolver
  "Syntax-quote resolution as Clojure does it for java.lang classes."
  [ctx sym]
  (if-let [c (and (nil? (namespace sym))
                  (try (Class/forName (str "java.lang." (name sym))) (catch Throwable _ nil)))]
    (symbol (.getName ^Class c))
    (forms/default-resolve-symbol ctx sym)))

(deftest reads-what-clojure-reads
  ;; Differential: Clojure's own reader is the oracle for everything both
  ;; languages share. Namespaces chosen because they use no imports, which only
  ;; the analyzer's resolver can know about.
  (doseq [[f ns] [["../../clojure/replique-clj/src/clj/clojure/string.clj" 'clojure.string]
                  ["../../clojure/replique-clj/src/clj/clojure/set.clj" 'clojure.set]
                  ["../../clojure/replique-clj/src/clj/clojure/walk.clj" 'clojure.walk]
                  ["../../clojure/replique-clj/src/clj/clojure/zip.clj" 'clojure.zip]]
          :when (.exists (io/file f))]
    (let [src (slurp f)
          ours (map :form (:forms (r/read-source src {:ns ns :core-ns 'clojure.core
                                                      :features #{:clj}
                                                      :resolve-symbol java-lang-resolver})))
          rdr (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. src))
          theirs (binding [*ns* (create-ns ns)]
                   (loop [acc []]
                     (let [x (read {:eof ::eof :read-cond :allow :features #{:clj}} rdr)]
                       (if (= ::eof x) acc (recur (conj acc x))))))]
      (is (= (count theirs) (count ours)) f)
      (doseq [[a b] (map vector ours theirs)]
        (is (= (normalize b) (normalize a)) f)))))
