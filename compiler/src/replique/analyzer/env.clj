(ns replique.analyzer.env
  "The compiler environment: what the compiler knows about namespaces and
  vars between top-level forms (doc/compilation.md §1). One explicit value,
  in an atom, rather than dynamic vars, so that two environments can live in
  one process and namespaces can be compiled in parallel.

    {:namespaces {ns-sym namespace}}

  A namespace:

    :name      the symbol
    :vars      {simple-sym var}, what the namespace defines
    :aliases   {alias-sym ns-sym}
    :imports   {simple-sym class-name-sym}, JVM classes
    :excludes  core names the namespace doesn't refer
    :requires  the namespaces it requires
    :options   the compiler options of its ns form (doc/language.md §3)

  A var is its compile-time metadata, which is never emitted
  (doc/language.md §2.3):

    :var        the qualified symbol
    :defined?   false for a var only declared
    :macro?     :macro-fn, the function the expander calls (doc/reader.md §4)
    :dynamic? :redef? :private? :doc :arglists :span
    :arities    {:fixed #{n ...} :variadic n?} when the init is a fn
    :type       the type of the value, when known
    :return-type  for a function, from a ^hint on its name"
  (:refer-clojure :exclude [find-ns find-var]))

(def core-ns 'replique.core)

(defn create
  "A new, empty environment."
  []
  (atom {:namespaces {}}))

(defn find-ns [cenv ns-sym]
  (get-in @cenv [:namespaces ns-sym]))

(defn- new-ns [ns-sym]
  {:name ns-sym :vars {} :aliases {} :imports {} :excludes #{}
   :requires #{} :options {}})

(defn ensure-ns!
  "The namespace `ns-sym`, created empty if it doesn't exist."
  [cenv ns-sym]
  (or (find-ns cenv ns-sym)
      (get-in (swap! cenv update-in [:namespaces ns-sym] #(or % (new-ns ns-sym)))
              [:namespaces ns-sym])))

(defn declare-ns!
  "Replace what an ns form declares about `ns-sym` (its aliases, imports,
  excludes, requires and options), keeping its vars: re-evaluating
  an ns form at the REPL must not forget the namespace's definitions."
  [cenv ns-sym decl]
  (swap! cenv update-in [:namespaces ns-sym]
         (fn [n] (merge (or n (new-ns ns-sym))
                        (select-keys decl [:aliases :imports :excludes
                                           :requires :options])))))

(defn find-var
  "The var named by the qualified symbol `q`, or nil."
  [cenv q]
  (when-let [ns (namespace q)]
    (get-in @cenv [:namespaces (symbol ns) :vars (symbol (name q))])))

(defn define-var!
  "Record a var. A definition (`info` with :defined? true) replaces what an
  earlier definition said, so that removing ^:private or a doc string from
  the source takes effect; a declaration keeps an existing definition.
  What other forms said of the var stays: :assigned?, from a set! of it
  elsewhere, which a redefinition doesn't take back (replique-cljs b40858f)."
  [cenv q info]
  (let [ns-sym (symbol (namespace q))
        sym (symbol (name q))
        info (assoc info :var q)]
    (swap! cenv update-in [:namespaces ns-sym]
           (fn [n]
             (let [n (or n (new-ns ns-sym))
                   old (get-in n [:vars sym])]
               (assoc-in n [:vars sym]
                         (if (:defined? info)
                           (merge (select-keys old [:assigned?]) info)
                           (merge info old))))))
    (find-var cenv q)))

(defn update-var!
  "Apply `f` to the var named `q`, which exists."
  [cenv q f & args]
  (swap! cenv update-in [:namespaces (symbol (namespace q)) :vars (symbol (name q))]
         #(apply f % args)))

(defn resolve-ns
  "The namespace an alias or a namespace name stands for in `ns-sym`, or nil."
  [cenv ns-sym alias]
  (let [here (find-ns cenv ns-sym)]
    (or (get-in here [:aliases alias])
        (when (find-ns cenv alias) alias))))

(defn resolve-var
  "The var `sym` names in namespace `ns-sym`, or nil. A simple symbol is, in
  order: a var of the namespace, a var of replique.core that the namespace
  doesn't exclude. There is no :refer (doc/language.md §2.5)."
  [cenv ns-sym sym]
  (if-let [n (namespace sym)]
    (when-let [target (resolve-ns cenv ns-sym (symbol n))]
      (find-var cenv (symbol (str target) (name sym))))
    (let [here (find-ns cenv ns-sym)]
      (or (get-in here [:vars sym])
          (when-not (contains? (:excludes here) sym)
            (find-var cenv (symbol (str core-ns) (name sym))))))))
