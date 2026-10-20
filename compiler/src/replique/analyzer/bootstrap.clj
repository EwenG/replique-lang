(ns replique.analyzer.bootstrap
  "What replique.core holds before any of core is compiled: the kernel's
  functions, declared, and the bootstrap macros.

  Core's macros will be written in Replique (doc/runtime.md), and a Replique
  macro runs once the JVM back end has compiled it. Until then, and while
  core itself is being compiled, these macros stand in for the ones the
  special forms can't do without. They are Clojure functions on the
  compiler's data (replique.analyzer.macros, :bootstrap), and they don't
  destructure: destructuring arrives with core (M2)."
  (:require [replique.analyzer.env :as env]
            [replique.analyzer.macros :as mx]))

(def kernel-fns
  "The functions of replique.core the host kernel provides (doc/runtime.md
  §2), and the ones syntax-quote expands into. Declared here so code can
  refer to them before the kernel exists; the kernel defines them."
  '[seq first rest next cons conj concat list list* vector hash-map hash-set
    apply with-meta meta vary-meta deref identity
    = not= < > <= >= + - * / inc dec nil? not some? str keyword symbol
    count get nth assoc
    ex-info ex-data ex-message ex-cause
    instance? print println prn pr-str
    eval])

(defn- fail [message]
  (throw (ex-info message {})))

(defn- check-symbols [what syms]
  (doseq [s syms]
    (when-not (symbol? s)
      (fail (str what " takes symbols here, got " (pr-str s)
                 (when (coll? s) "; destructuring arrives with core (M2)"))))))

(defn- check-bindings [what bindings]
  (when-not (and (vector? bindings) (even? (count bindings)))
    (fail (str what " takes a vector of an even number of forms")))
  (check-symbols what (take-nth 2 bindings)))

(defn- sigs
  "The methods of a fn: ([params] body...) lists, from either the single
  arity sugar or the lists themselves."
  [what more]
  (let [ms (if (vector? (first more)) [(apply list more)] more)]
    (doseq [m ms]
      (when-not (and (seq? m) (vector? (first m)))
        (fail (str what " takes a parameter vector and a body, or lists of them")))
      (check-symbols what (first m)))
    ms))

;; --- the macros ------------------------------------------------------------

(defn- fn-macro [&form _ & more]
  (let [name (when (symbol? (first more)) (first more))
        more (if name (rest more) more)]
    (with-meta (list* 'fn* (concat (when name [name]) (sigs "fn" more)))
      (meta &form))))

(defn- let-macro [_ _ bindings & body]
  (check-bindings "let" bindings)
  (list* 'let* bindings body))

(defn- loop-macro [_ _ bindings & body]
  (check-bindings "loop" bindings)
  (list* 'loop* bindings body))

(defn- defn-parts
  "[name' sigs] for (defn name doc? attrs? sigs...)."
  [what name more extra-meta]
  (when-not (symbol? name) (fail (str what " takes a name")))
  (let [[doc more] (if (string? (first more)) [(first more) (rest more)] [nil more])
        [attrs more] (if (map? (first more)) [(first more) (rest more)] [nil more])
        ms (sigs what more)]
    [(vary-meta name merge attrs extra-meta
                (cond-> {:arglists (mapv first ms)} doc (assoc :doc doc)))
     ms]))

(defn- defn-macro [_ _ name & more]
  (let [[name ms] (defn-parts "defn" name more nil)]
    (list 'def name (list* 'replique.core/fn ms))))

(defn- defn--macro [_ _ name & more]
  (let [[name ms] (defn-parts "defn-" name more {:private true})]
    (list 'def name (list* 'replique.core/fn ms))))

(defn- defmacro-macro [_ _ name & more]
  (let [[name ms] (defn-parts "defmacro" name more {:macro true})]
    (list 'def name
          (list* 'replique.core/fn
                 (map (fn [[params & body]] (list* (into '[&form &env] params) body)) ms)))))

(defn- declare-macro [_ _ & names]
  (check-symbols "declare" names)
  (list* 'do (map #(list 'def (vary-meta % assoc :declared true)) names)))

(defn- when-macro [_ _ test & body] (list 'if test (list* 'do body)))
(defn- when-not-macro [_ _ test & body] (list 'if test nil (list* 'do body)))
(defn- if-not-macro
  ([_ _ test then] (list 'if test nil then))
  ([_ _ test then else] (list 'if test else then)))

(defn- cond-macro [_ _ & clauses]
  (when (odd? (count clauses)) (fail "cond takes an even number of forms"))
  (when (seq clauses)
    (list 'if (first clauses) (second clauses)
          (list* 'replique.core/cond (nnext clauses)))))

(defn- and-macro [_ _ & xs]
  (cond
    (empty? xs) true
    (empty? (rest xs)) (first xs)
    :else (let [g (mx/gensym "and")]
            (list 'let* [g (first xs)] (list 'if g (list* 'replique.core/and (rest xs)) g)))))

(defn- or-macro [_ _ & xs]
  (cond
    (empty? xs) nil
    (empty? (rest xs)) (first xs)
    :else (let [g (mx/gensym "or")]
            (list 'let* [g (first xs)] (list 'if g g (list* 'replique.core/or (rest xs)))))))

(defn- comment-macro [_ _ & _] nil)

(defn- case-macro [_ _ e & clauses]
  (let [default? (odd? (count clauses))
        pairs (partition 2 clauses)
        tests (mapv (fn [[t _]] (if (seq? t) (vec t) [t])) pairs)
        thens (mapv second pairs)]
    (concat (list 'case* e tests thens) (when default? [(last clauses)]))))

(defn- letfn-macro [_ _ fnspecs & body]
  (when-not (and (vector? fnspecs) (every? #(and (seq? %) (symbol? (first %))) fnspecs))
    (fail "letfn takes a vector of (name [params] body...) specs"))
  (list* 'letfn*
         (vec (mapcat (fn [[n & more]] [n (list* 'replique.core/fn n more)]) fnspecs))
         body))

(defn- thread [x forms last?]
  (reduce (fn [acc f]
            (if (seq? f)
              (with-meta (if last?
                           (concat f [acc])
                           (list* (first f) acc (rest f)))
                (meta f))
              (list f acc)))
          x forms))

(defn- thread-first [_ _ x & forms] (thread x forms false))
(defn- thread-last [_ _ x & forms] (thread x forms true))

(def macros
  {'fn fn-macro 'let let-macro 'loop loop-macro 'defn defn-macro 'defn- defn--macro
   'defmacro defmacro-macro 'declare declare-macro 'when when-macro
   'when-not when-not-macro 'if-not if-not-macro 'cond cond-macro 'and and-macro
   'or or-macro 'comment comment-macro 'case case-macro 'letfn letfn-macro
   '-> thread-first '->> thread-last})

(defn install!
  "Declare the kernel's functions and the bootstrap macros in replique.core."
  [cenv]
  (env/ensure-ns! cenv env/core-ns)
  (doseq [s kernel-fns]
    (env/define-var! cenv (symbol (str env/core-ns) (str s)) {:defined? true :kernel? true}))
  (doseq [[s f] macros]
    (env/define-var! cenv (symbol (str env/core-ns) (str s))
                     {:defined? true :macro? true :macro-kind :bootstrap :macro-fn f}))
  cenv)
