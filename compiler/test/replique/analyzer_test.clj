(ns replique.analyzer-test
  (:require [clojure.test :refer [are deftest is testing]]
            [replique.analyzer :as a]
            [replique.analyzer.env :as env]
            [replique.analyzer.host :as host]
            [replique.analyzer.macros :as mx]
            [replique.analyzer.types :as t]
            [replique.ir :as ir]))

(defn- units
  "The records of every form of `src`, in a fresh environment unless one is
  given."
  [src & {:keys [cenv] :as opts}]
  (:units (a/analyze-source (or cenv (a/environment)) src
                            (merge {:file "t.rpq" :ns 'app.core} (dissoc opts :cenv)))))

(defn- unit [src & opts] (last (apply units src opts)))
(defn- root [src & opts] (:node (apply unit src opts)))

(defn- codes [src & opts]
  (mapv :code (mapcat :diagnostics (apply units src opts))))

(defn- error [src & opts]
  (first (filter #(= :error (:severity %)) (mapcat :diagnostics (apply units src opts)))))

(defn- find-op [n op] (first (filter #(= op (:op %)) (ir/nodes n))))
(defn- find-ops [n op] (filter #(= op (:op %)) (ir/nodes n)))
(defn- pos [n] (select-keys (:span n) [:line :column :end-line :end-column]))

(deftest special-forms
  (testing "if, with nil for a missing else"
    (let [n (root "(if true 1)")]
      (is (= [:if :const :const :const] (map :op (ir/nodes n))))
      (is (nil? (:val (:else n))))))
  (testing "do"
    (let [n (root "(do 1 2 3)")]
      (is (= [1 2] (map :val (:statements n))))
      (is (= 3 (:val (:ret n))))))
  (testing "let*: each init sees the bindings before it"
    (let [n (root "(let* [x 1 y x] y)")
          [bx by] (:bindings n)]
      (is (= (:id bx) (:id (:init by))))
      (is (= (:id by) (:id (:body n))))
      (is (= :let (:local bx)))))
  (testing "loop* and recur"
    (let [n (root "(loop* [i 0] (if i (recur i) i))")]
      (is (= (:id n) (:target (find-op n :recur))))
      (is (:recurs? n))
      (is (false? (:recurs? (root "(loop* [i 0] i)"))))))
  (testing "fn*: arities, rest parameter, self name"
    (let [n (root "(fn* f ([] (f 1)) ([x & xs] x))")
          [m0 m1] (:methods n)]
      (is (= [0 1] (map :fixed-arity (:methods n))))
      (is (:variadic? n))
      (is (= :fn (:local (:local n))))
      (is (= (:id (:local n)) (:id (:fn (:body m0)))) "the self name is in scope")
      (is (= '[x xs] (map :name (:params m1))))))
  (testing "quote removes the reader's metadata"
    (let [v (:val (:expr (root "'(a [1 2] {:k s})")))]
      (is (= '(a [1 2] {:k s}) v))
      (is (every? nil? (map meta (tree-seq coll? seq v))))
      (is (nil? (meta (first v))))))
  (testing "def returns nil and declares before its init"
    (let [n (root "(def f (fn* [] (f)))")]
      (is (= t/nil-type (:type n)))
      (is (= 'app.core/f (:var n) (:var (:fn (:body (first (:methods (:init n))))))))
      (is (:top-level? n))))
  (testing "var"
    (is (= {:op :the-var :var 'app.core/x}
           (select-keys (root "(def x 1) (var x)") [:op :var]))))
  (testing "throw"
    (is (= t/never (:type (root "(throw 1)")))))
  (testing "try, catch and finally"
    (let [n (root "(try 1 (catch Exception e e) (catch :default e 2) (finally 3))")
          [c1 c2] (:catches n)]
      (is (= Exception (:val (:class c1))))
      (is (nil? (:class c2)) ":default catches everything")
      (is (= 3 (:val (:finally n))))
      (is (= (:id (:local c1)) (:id (:body c1)))))
    (is (= :analyzer/bad-try (:code (error "(try 1 (catch :default e 1) (catch Exception e 2))")))))
  (testing "set! on a var"
    (let [cenv (a/environment)
          n (root "(def x 1) (set! x 2)" :cenv cenv)]
      (is (= :var (:op (:target n))))
      (is (:assigned? (env/find-var cenv 'app.core/x)))))
  (testing "case*"
    (let [n (root "(case* 1 [[1 2] [:a]] [:x :y] :z)")]
      (is (= [[1 2] [:a]] (map #(map :val (:tests %)) (:nodes n))))
      (is (= :z (:val (:default n)))))
    (is (not (contains? (root "(case* 1 [[1]] [:x])") :default)) "no default: no match throws"))
  (testing "letfn*: every name is in scope in every init"
    (let [n (root "(letfn* [f (fn* [] (g)) g (fn* [] (f))] (f))")
          [bf bg] (:bindings n)]
      (is (= (:id bg) (:id (:fn (:body (first (:methods (:init bf)))))))))))

(deftest collections
  (is (= [:vector :const :local] (map :op (ir/nodes (:body (root "(let* [x 1] [1 x])"))))))
  (is (= [:a :b] (map :val (:keys (root "{:a 1 :b 2}")))))
  (testing "metadata on a literal is part of the value"
    (let [n (root "^:m [1]")]
      (is (= :with-meta (:op n)))
      (is (= [:m] (map :val (:keys (:meta n)))))))
  (testing "the reader's metadata is not"
    (is (= :vector (:op (root "[1]"))))))

(deftest spans
  (testing "a node written by hand has its own span"
    (let [n (root "(if x\n  1 \"s\")" :cenv (doto (a/environment)
                                                   (env/define-var! 'app.core/x {:defined? true})))]
      (is (= {:line 1 :column 1 :end-line 2 :end-column 9} (pos n)))
      (is (= {:line 1 :column 5 :end-line 1 :end-column 6} (pos (:test n))))
      (is (= {:line 2 :column 3 :end-line 2 :end-column 4} (pos (:then n))) "constants too")
      (is (= {:line 2 :column 5 :end-line 2 :end-column 8} (pos (:else n))))
      (is (every? nil? (map :origin (ir/nodes n))))
      (is (= "t.rpq" (:file (:span n))))))
  (testing "a node a macro built gets the call's span and the expansion"
    (let [{:keys [node expansions]} (unit "(when true\n  (f))" :cenv (doto (a/environment)
                                                                   (env/define-var! 'app.core/f {:defined? true})))
          [id entry] (first expansions)]
      (is (= 'replique.core/when (:macro entry)))
      (is (= {:line 1 :column 1 :end-line 2 :end-column 7} (pos node)))
      (is (= id (:origin node)))
      (is (= (:span node) (:call entry)))
      (testing "except what was passed to it unchanged"
        (let [call (:ret (:then node))]
          (is (= :invoke (:op call)))
          (is (nil? (:origin call)))
          (is (= {:line 2 :column 3 :end-line 2 :end-column 6} (pos call)))))))
  (testing "expansions inside expansions record their parent"
    (let [{:keys [expansions]} (unit "(defn f [] 1)")
          by-macro (into {} (map (fn [[id e]] [(:macro e) (assoc e :id id)])) expansions)]
      (is (= #{'replique.core/defn 'replique.core/fn} (set (keys by-macro))))
      (is (= (:id (by-macro 'replique.core/defn)) (:parent (by-macro 'replique.core/fn))))
      (is (nil? (:parent (by-macro 'replique.core/defn)))))))

(deftest ids
  (testing "unique within a form, and the same on every compile"
    (let [src "(defn f [x] (let [y x] (loop [i y] (if i (recur i) (fn [z] z)))))"
          n1 (root src)
          n2 (root src)
          ids (keep #(when (#{:binding :loop :fn-method} (:op %)) (:id %)) (ir/nodes n1))]
      (is (= ids (distinct ids)))
      (is (= n1 n2)))))

(deftest vars-and-namespaces
  (testing "what a form defines and consumes"
    (let [[_ u] (units "(def x 1) (defn f [] (x))")]
      (is (= ['app.core/f] (:defines u)))
      (is (= #{'app.core/x} (:vars (:inputs u))))
      (is (= #{'replique.core/defn 'replique.core/fn} (:macros (:inputs u))))))
  (testing "var metadata stays in the compiler environment"
    (let [cenv (a/environment)]
      (units "(defn ^:private ^String f \"doc\" ([x] x) ([x & ys] x))" :cenv cenv)
      (let [v (env/find-var cenv 'app.core/f)]
        (is (:private? v))
        (is (= "doc" (:doc v)))
        (is (= '[[x] [x & ys]] (:arglists v)))
        (is (= {:fixed #{1} :variadic 1} (:arities v)))
        (is (= String (:base (:return-type v)))))))
  (testing "a redefinition replaces what the last one said"
    (let [cenv (a/environment)]
      (units "(defn- f [] 1) (defn f [] 2)" :cenv cenv)
      (is (not (:private? (env/find-var cenv 'app.core/f))))))
  (testing "but not what a set! elsewhere said of it"
    (let [cenv (a/environment)]
      (units "(def ^:redef x 1) (defn g [] (set! x 2)) (def ^:redef x 3)" :cenv cenv)
      (is (:assigned? (env/find-var cenv 'app.core/x)))))
  (testing "a def of a core name takes the name over"
    (let [cenv (a/environment)
          [_ u] (units "(def inc 1) inc" :cenv cenv)]
      (is (= 'app.core/inc (:var (:node u))))))
  (testing "ns: aliases"
    (let [cenv (a/environment)]
      (units "(ns lib.a) (def x 1) (def ^:private z 3)" :cenv cenv)
      (let [[n u1] (units "(ns app.b (:require [lib.a :as a])) a/x" :cenv cenv)]
        (is (= '{:name app.b :requires [lib.a]} (select-keys (:node n) [:name :requires])))
        (is (= 'app.b (:ns u1)))
        (is (= 'lib.a/x (:var (:node u1)))))
      (is (= :analyzer/private-var (:code (error "(ns app.c (:require [lib.a :as a])) a/z" :cenv cenv))))
      (is (= :analyzer/unknown-namespace (:code (error "(ns app.d (:require [lib.nope]))" :cenv cenv))))))
  (testing "ns: there is no :refer"
    (let [cenv (a/environment)]
      (units "(ns lib.a) (def x 1)" :cenv cenv)
      (let [e (error "(ns app.e (:require [lib.a :as a :refer [x]]))" :cenv cenv)]
        (is (= :analyzer/bad-ns (:code e)))
        (is (re-find #":refer isn't supported" (:message e))))
      (is (= :analyzer/bad-ns (:code (error "(ns app.e (:require [lib.a :refer :all]))" :cenv cenv))))
      (is (= :analyzer/bad-ns (:code (error "(ns app.e (:use lib.a))" :cenv cenv))))))
  (testing "syntax-quote resolves through the environment"
    (let [cenv (a/environment)]
      (units "(ns lib.a) (def x 1)" :cenv cenv)
      (is (= '[lib.a/x replique.core/inc app.f/local java.lang.String]
             (map (comp :val :expr :node)
                  (rest (units "(ns app.f (:require [lib.a :as a])) `a/x `inc `local `String"
                               :cenv cenv)))))))
  (testing "requires, for the driver"
    (is (= '[a b] (a/ns-requires '(ns x (:require a [b :as c] [d :as-alias e])))))))

(deftest errors
  (are [src code] (= code (:code (error src)))
    "(recur 1)" :analyzer/recur-outside
    "(loop* [x 1] (recur 1 2))" :analyzer/wrong-arguments
    "(loop* [x 1] (do (recur 1) 2))" :analyzer/recur-not-tail
    "(loop* [x 1] (try (recur 1)))" :analyzer/recur-across-try
    "(if)" :analyzer/wrong-arguments
    "nope" :analyzer/unresolved-symbol
    "(let [[a] 1] a)" :analyzer/macro-failed
    "(let* [[a] 1] a)" :analyzer/bad-binding
    "(fn* ([x] 1) ([y] 2))" :analyzer/bad-fn
    "(def a/b 1)" :analyzer/bad-def
    "(let* [x 1] (set! x 2))" :analyzer/bad-set!
    "(catch Exception e 1)" :analyzer/bad-try
    "(case* 1 [[1] [1]] [:a :b])" :analyzer/bad-case
    "(fn* [] (await 1))" :analyzer/await-outside-async
    "(deftype* T [])" :analyzer/not-implemented
    "(defmacro m [] 1) (m)" :analyzer/macro-not-loaded
    "(fn* [] (ns x))" :analyzer/bad-ns
    "(js* \"1\")" :analyzer/js-only
    "when" :analyzer/macro-value)
  (testing "an error has the span of the form it is about"
    (is (= {:line 2 :column 4 :end-line 2 :end-column 8}
           (select-keys (:span (error "(do 1\n   nope)")) [:line :column :end-line :end-column]))))
  (testing "a form with an error doesn't stop the next one"
    (let [[u1 u2] (units "nope 1")]
      (is (nil? (:node u1)))
      (is (= 1 (:val (:node u2)))))))

(deftest types
  (are [src base] (= base (:base (:type (root src))))
    "1" 'long "1.5" 'double "\"s\"" String ":k" :keyword "[1]" :vector
    "(if true 1 2)" 'long
    "(if true 1 1.5)" Number
    "(if true 1 \"s\")" :any
    "(let* [^String s nil] s)" String
    "(fn* [])" :fn)
  (is (= (t/of Long true) (:type (root "(if true 1 nil)"))) "a primitive or nil is boxed")
  (testing "a loop local takes its init's type, widened to cover what recur passes"
    (is (= 'long (:base (:type (first (:bindings (root "(loop* [i 0] (if true (recur 1) i))")))))))
    (is (= Number (:base (:type (first (:bindings (root "(loop* [i 0] (if true (recur 1.5) i))")))))))
    (let [u (unit "(loop* [i 0] (if (when true 1) (recur 1.5) i))")]
      (is (= 1 (count (:expansions u))) "the second analysis doesn't keep the first one's expansions")))
  (testing "JS"
    (are [src base] (= base (:base (:type (root src :target :js))))
      "1" 'number "1.5" 'number "1N" 'bigint "\"s\"" 'string "\\c" 'string)))

(deftest interop
  (testing "members are resolved on the JVM"
    (let [n (root "(let* [s \"a\"] (.length s))")]
      (is (= {:class String :name "length" :descriptor "()I" :static? false :interface? false}
             (:jvm/member (:body n))))
      (is (= 'int (:base (:type (:body n))))))
    (is (= "(J)J" (:descriptor (:jvm/member (root "(Math/abs 1)")))) "the most specific overload")
    (is (= "(Ljava/lang/String;)V" (:descriptor (:jvm/member (root "(StringBuilder. \"x\")")))))
    (is (= :host-field (:op (root "Long/MAX_VALUE"))))
    (is (= {:op :qualified-method :kind :instance}
           (select-keys (root "String/.length") [:op :kind])))
    (is (= Integer (:val (root "Integer")))))
  (testing "reflection is a warning in dev mode, an error in advanced mode"
    (is (= [:analyzer/reflection] (codes "(fn* [x] (.length x))")))
    (is (= :analyzer/reflection (:code (error "(fn* [x] (.length x))" :mode :advanced)))))
  (testing "imports"
    (is (= java.util.UUID (:val (root "(ns app.i (:import [java.util UUID])) UUID")))))
  (testing "JS: members are names"
    (let [n (root "(.log js/console 1)" :target :js)]
      (is (= {:op :host-call :method 'log} (select-keys n [:op :method])))
      (is (= {:op :js/global :name "console"} (select-keys (:target n) [:op :name])))
      (is (not (contains? n :jvm/member))))
    (let [n (root "(js/Date. 0)" :target :js)]
      (is (= :new (:op n)) "a constructor sugar on a js/ name")
      (is (= {:op :js/global :name "Date"} (select-keys (:class n) [:op :name]))))
    (is (= :js/object (:op (root "#js {:a 1}" :target :js))))
    (is (= ["" " + " ""] (:template (root "(js* \"~{} + ~{}\" 1 2)" :target :js))))))

(deftest numbers-by-target
  (testing "case tests are distinct as the target compares them"
    (is (empty? (codes "(case* 1 [[1] [1.0]] [:a :b])")))
    (is (= :analyzer/bad-case (:code (error "(case* 1 [[1] [1.0]] [:a :b])" :target :js))))))

(deftest passes
  (testing "a fn closes over the outer locals it uses"
    (let [n (root "(let* [a 1 b 2] (fn* [c] (fn* [] [a c])))")
          [ba] (:bindings n)
          outer (find-op n :fn)
          inner (second (find-ops n :fn))]
      (is (= [(:id ba)] (:closed-over outer)))
      (is (= [(:id ba) (:id (first (:params (first (:methods outer)))))] (:closed-over inner))))))

(deftest macros
  (let [cenv (a/environment)
        defmacro! (fn [sym f] (env/define-var! cenv sym {:defined? true :macro? true
                                                          :macro-kind :bootstrap :macro-fn f}))]
    (defmacro! 'app.core/env-keys (fn [_ env] (list 'quote (vec (sort (keys env))))))
    (defmacro! 'app.core/a-fn (fn [_ _] [1 {:k (fn [])}]))
    (defmacro! 'app.core/gs (fn [_ _] (list 'quote [(mx/gensym "x") (mx/gensym "x")])))
    (testing "&env holds the locals in scope"
      (is (= '[a b] (:val (:expr (:body (root "(let* [a 1 b 2] (env-keys))" :cenv cenv)))))))
    (testing "a value that can't be a literal is an error with its path"
      (let [e (error "(a-fn)" :cenv cenv)]
        (is (= :analyzer/cannot-embed (:code e)))
        (is (= [1 :k] (:path e)))))
    (testing "gensyms are numbered per file"
      (let [[x1 x2] (:val (:expr (root "(gs)" :cenv cenv)))]
        (is (not= x1 x2))
        (is (= [x1 x2] (:val (:expr (root "(gs)" :cenv cenv)))))))))

(deftest bootstrap-macros
  (are [src v] (= v (:val (root src)))
    "(and)" true "(or)" nil "(comment x)" nil "(and 1)" 1)
  (is (= [:let :binding :const :if :local :local :const] (map :op (ir/nodes (root "(or 1 2)"))))))
