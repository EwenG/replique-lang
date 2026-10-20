(ns replique.analyzer
  "Forms to the IR of doc/ir.md, one top-level form at a time.

    (analyze-top cenv form opts)   one form, already read
    (analyze-source cenv src opts) every form of a source, read and analysed
                                   in order, so that an ns form changes how
                                   the forms after it read

  The analyzer's working state is an explicit environment, `env`, threaded
  through the calls and never stored on nodes (doc/ir.md §1):

    :ns          the current namespace
    :locals      {sym binding}, where a binding is {:id :name :local :type}
    :tail?       whether the form is in tail position, for recur
    :recur       the recur target {:id :arity :types}, :barrier inside a
                 try, or nil
    :top-level?  true until a fn or a let encloses the form; do keeps it
    :async?      inside an ^:async fn, where await is allowed
    :pos         {:span :origin}: the span and expansion a node gets when
                 its form carries no span of its own

  What one top-level form collects (ids, expansions, inputs, warnings) is in
  its unit, replique.analyzer.unit.

  Spans (doc/reader.md §3.1). A form with its own span was written by hand:
  its node gets that span and no :origin. A form without one was built by a
  macro and gets the span of the macro call and the expansion's id; so does a
  form that a macro gave the call's own metadata, as `fn` does. A constant
  gets the span its collection recorded for it, when the collection was read
  from source."
  (:require [clojure.string :as str]
            [replique.analyzer.bootstrap :as bootstrap]
            [replique.analyzer.env :as env]
            [replique.analyzer.host :as host]
            [replique.analyzer.macros :as mx]
            [replique.analyzer.passes :as passes]
            [replique.analyzer.types :as t]
            [replique.analyzer.unit :as u]
            [replique.reader :as reader]
            [replique.reader.cst :as cst]
            [replique.reader.forms :as forms])
  (:import [clojure.lang IObj TaggedLiteral]
           [java.time Instant]
           [java.util UUID]
           [java.util.regex Pattern]))

(declare analyze class-const)

;; --- spans and nodes -------------------------------------------------------

(def ^:private span-keys [:line :column :end-line :end-column :file])

(defn- own-span [form]
  (when (instance? IObj form)
    (let [m (meta form)]
      (when (:line m) (select-keys m span-keys)))))

(defn- enter
  "The env for analysing `form`: a form with a span of its own, other than
  the span of the macro call being expanded, was written by hand."
  [env form]
  (let [s (own-span form)]
    (if (and s (not= s (:span (:pos env))))
      (assoc env :pos {:span s :origin nil})
      env)))

(defn- at
  "The env for the element `k` of `coll`: the span the reader recorded for
  it, when it can't carry its own."
  [env coll k x]
  (if (own-span x)
    env
    (if-let [s (reader/element-span coll k)]
      (assoc env :pos {:span (if (map? coll) (second s) s) :origin nil})
      env)))

(defn- at-key [env coll k]
  (if (own-span k)
    env
    (if-let [[ks] (reader/element-span coll k)]
      (assoc env :pos {:span ks :origin nil})
      env)))

(defn- span-of [env form]
  (let [s (own-span form)]
    (if (and s (not= s (:span (:pos env)))) s (:span (:pos env)))))

(defn- fail [env form code message & more]
  (apply u/fail (span-of env form) code message more))

(defn- warn! [u env form code message]
  (u/warn! u (span-of env form) code message))

(defn- node
  "A node of op `op` for `form`, with the keys of doc/ir.md §1."
  [env form op type m]
  (merge {:op op :form form :span (:span (:pos env)) :origin (:origin (:pos env))
          :type type :children []}
         m))

(defn- not-tail [env] (assoc env :tail? false))

(defn- sub
  "Analyse the element at index `i` of the list or vector `coll`."
  [u env coll i]
  (let [x (nth coll i)]
    (analyze u (at env coll i x) x)))

(defn- subs-from
  "Analyse the elements of `coll` from index `i`, not in tail position."
  [u env coll i]
  (let [env (not-tail env)]
    (mapv #(sub u env coll %) (range i (count coll)))))

(defn- arg-types [nodes] (mapv :type nodes))

;; --- constants -------------------------------------------------------------

(defn- const-kind [v]
  (cond
    (nil? v) :nil (boolean? v) :boolean (number? v) :number (string? v) :string
    (char? v) :char (keyword? v) :keyword (symbol? v) :symbol
    (instance? Pattern v) :regex (instance? UUID v) :uuid (instance? Instant v) :inst
    (class? v) :class (vector? v) :vector (map? v) :map (set? v) :set (seq? v) :list))

(defn program-value
  "`x` without the reader's metadata, at every level: the value a quoted
  constant has in the program."
  [x]
  (let [m (some-> (meta x) (#(apply dissoc % forms/reader-keys)) not-empty)
        v (cond
            (map-entry? x) x
            (seq? x) (apply list (map program-value x))
            (vector? x) (mapv program-value x)
            (map? x) (into {} (map (fn [[k v]] [(program-value k) (program-value v)])) x)
            (set? x) (into #{} (map program-value) x)
            :else x)]
    (if (instance? IObj v)
      (with-meta v (some-> m program-value))
      v)))

(defn- check-constant! [u env form v]
  (when-let [[path x] (mx/unembeddable v)]
    (fail env form :analyzer/cannot-embed
          (str "a " (.getName (class x)) (when (seq path) (str " at " (pr-str path)))
               " can't be emitted as a literal")
          :path path))
  (when (= :js (host/target (:host u)))
    (doseq [x (tree-seq coll? seq v)
            :when (or (ratio? x) (decimal? x))]
      (fail env form :analyzer/invalid-number
            (str (pr-str x) " has no JS equivalent (doc/language.md §5)")))))

(defn- const [u env form v]
  (check-constant! u env form v)
  (node env form :const (host/value-type (:host u) v)
        {:val v :kind (const-kind v)}))

;; --- types from hints ------------------------------------------------------

(defn- imports [u env]
  (:imports (env/find-ns (:cenv u) (:ns env))))

(defn- hint
  "The type the ^tag on `form` gives, or nil. An unknown tag is a warning."
  [u env form]
  (when-let [tag (:tag (meta form))]
    (or (host/hint-type (:host u) (imports u env) tag)
        (do (warn! u env form :analyzer/unknown-hint
                   (str "the type hint " tag " names no type"))
            nil))))

;; --- locals ----------------------------------------------------------------

(defn- binding-node
  "A :binding for `sym`, of kind `local`, and the env with it in scope."
  [u env sym local type & {:as more}]
  (when-not (symbol? sym)
    (fail env sym :analyzer/bad-binding
          (str "a binding is a symbol, got " (pr-str sym)
               (when (coll? sym) "; destructuring arrives with core (M2)"))))
  (when (namespace sym)
    (fail env sym :analyzer/bad-binding (str "a local can't be qualified: " sym)))
  (when (= '& sym)
    (fail env sym :analyzer/bad-binding "& is only valid in a parameter vector"))
  (let [env (enter env sym)
        b (merge {:id (u/next-id! u) :name sym :local local
                  :type (or (hint u env sym) type t/any)}
                 more)]
    [(node env sym :binding (:type b) (dissoc b :init))
     b]))

(defn- with-local [env b]
  (assoc-in env [:locals (:name b)] (select-keys b [:id :name :local :type :mutable?])))

(defn- local-node [env sym b]
  (node env sym :local (:type b) {:id (:id b) :name sym :local (:local b)}))

;; --- the simple special forms ----------------------------------------------

(defn- check-count [env form lo hi message]
  (let [n (dec (count form))]
    (when (or (< n lo) (and hi (> n hi)))
      (fail env form :analyzer/wrong-arguments message))))

(defn- parse-if [u env form]
  (check-count env form 2 3 "(if test then else?)")
  (let [test (sub u (not-tail env) form 1)
        then (sub u env form 2)
        else (if (= 4 (count form))
               (sub u env form 3)
               (node env nil :const t/nil-type {:val nil :kind :nil}))]
    (node env form :if (t/join (:type then) (:type else))
          {:test test :then then :else else :children [:test :then :else]})))

(defn- body
  "A :do of the forms of `coll` from index `i` to `j` (exclusive, default the
  end); the last is in the tail position `env` says."
  ([u env form coll i] (body u env form coll i (count coll)))
  ([u env form coll i j]
   (cond
     (>= i j) (node env form :const t/nil-type {:val nil :kind :nil})
     (= i (dec j)) (sub u env coll i)
     :else
     (let [stmts (mapv #(sub u (not-tail env) coll %) (range i (dec j)))
           ret (sub u env coll (dec j))]
       (node env form :do (:type ret)
             {:statements stmts :ret ret :children [:statements :ret]})))))

(defn- parse-do [u env form]
  (let [n (count form)]
    (if (= 1 n)
      (node env form :const t/nil-type {:val nil :kind :nil})
      (let [stmts (mapv #(sub u (not-tail env) form %) (range 1 (dec n)))
            ret (sub u env form (dec n))]
        (node env form :do (:type ret)
              {:statements stmts :ret ret :children [:statements :ret]})))))

(defn- binding-pairs [env form bindings what]
  (when-not (and (vector? bindings) (even? (count bindings)))
    (fail env form :analyzer/bad-binding
          (str what " takes a vector of an even number of forms"))))

(defn- parse-bindings
  "The bindings of a let* or loop*, left to right. `types` overrides the
  type of a binding by index (a loop's, widened to cover its recurs)."
  [u env form local types]
  (let [bv (second form)]
    (binding-pairs env form bv (str (first form)))
    (loop [i 0 env' env nodes []]
      (if (< i (count bv))
        (let [init (analyze u (at (not-tail env') bv (inc i) (nth bv (inc i))) (nth bv (inc i)))
              type (get types (quot i 2) (:type init))
              [bn b] (binding-node u (at env' bv i (nth bv i)) (nth bv i) local type)
              bn (assoc bn :init init :children [:init])]
          (recur (+ i 2) (with-local env' b) (conj nodes bn)))
        [nodes env']))))

(defn- parse-let [u env form]
  (let [[nodes env'] (parse-bindings u (assoc env :top-level? false) form :let nil)
        b (body u env' form form 2)]
    (node env form :let (:type b) {:bindings nodes :body b :children [:bindings :body]})))

(defn- snapshot [u]
  (into {} (map (fn [k] [k @(get u k)])) [:ids :expansions :inputs :defines :warnings]))

(defn- restore! [u snap]
  (doseq [[k v] snap] (vreset! (get u k) v)))

(defn- parse-loop
  "loop* is let* plus a recur target. A loop local takes the type of its
  init, so a loop over a long stays primitive; when a recur passes a value
  the type doesn't cover, the loop is analysed again with the types widened,
  as Clojure does."
  [u env form]
  (let [snap (snapshot u)]
    (loop [types nil]
      (let [mismatch (volatile! {})
            [nodes env'] (parse-bindings u (assoc env :top-level? false) form :loop types)
            id (u/next-id! u)
            b (body u (assoc env' :tail? true
                             :recur {:id id :arity (count nodes) :types (mapv :type nodes)
                                     :mismatch mismatch})
                    form form 2)]
        (if (seq @mismatch)
          (do (restore! u snap)
              (recur (reduce-kv (fn [ts i t] (assoc ts i (t/join (:type (nodes i)) t)))
                                (into {} (map-indexed (fn [i n] [i (:type n)])) nodes)
                                @mismatch)))
          (node env form :loop (:type b)
                {:id id :bindings nodes :body b :children [:bindings :body]}))))))

(defn- parse-recur [u env form]
  (let [target (:recur env)]
    (cond
      (= :barrier target) (fail env form :analyzer/recur-across-try "recur can't cross a try")
      (nil? target) (fail env form :analyzer/recur-outside "recur outside a loop or fn")
      (not (:tail? env)) (fail env form :analyzer/recur-not-tail "recur is only allowed in tail position"))
    (let [exprs (subs-from u env form 1)]
      (when-not (= (count exprs) (:arity target))
        (fail env form :analyzer/wrong-arguments
              (str "recur takes " (:arity target) " argument(s) here, got " (count exprs))))
      (doseq [[i e] (map-indexed vector exprs)
              :let [bt (get (:types target) i)]
              :when (and bt (:mismatch target) (not= bt (t/join bt (:type e))))]
        (vswap! (:mismatch target) update i #(t/join (or % t/never) (:type e))))
      (node env form :recur t/never
            {:target (:id target) :exprs exprs :children [:exprs]}))))

(defn- parse-throw [u env form]
  (check-count env form 1 1 "(throw exception)")
  (let [e (sub u (not-tail env) form 1)]
    (node env form :throw t/never {:exception e :children [:exception]})))

(defn- parse-quote [u env form]
  (check-count env form 1 1 "(quote form)")
  (let [x (second form)
        c (const u (at env form 1 x) x (program-value x))]
    (node env form :quote (:type c) {:expr c :children [:expr]})))

;; --- vars ------------------------------------------------------------------

(defn- current-ns [env] (:ns env))

(defn- var-ref!
  "The var `sym` resolves to, recorded as an input, or nil."
  [u env sym]
  (when-let [v (env/resolve-var (:cenv u) (current-ns env) sym)]
    (when (and (:private? v)
               (not= (namespace (:var v)) (str (current-ns env))))
      (fail env sym :analyzer/private-var (str (:var v) " is private")))
    (u/input! u :vars (:var v))
    v))

(defn- var-node [u env sym v]
  (when (:macro? v)
    (fail env sym :analyzer/macro-value (str "can't take the value of the macro " (:var v))))
  (node env sym :var (or (:type v) t/any) {:var (:var v)}))

(defn- parse-var [u env form]
  (check-count env form 1 1 "(var symbol)")
  (let [sym (second form)
        v (and (symbol? sym) (var-ref! u env sym))]
    (when-not v
      (fail env form :analyzer/unresolved-var (str "(var " (pr-str sym) ") names no var")))
    (node env form :the-var t/any {:var (:var v)})))

(defn- fn-arities [init]
  (when (= :fn (:op init))
    {:fixed (into #{} (map :fixed-arity) (remove :variadic? (:methods init)))
     :variadic (some #(when (:variadic? %) (:fixed-arity %)) (:methods init))}))

(def ^:private def-flags
  {:dynamic :dynamic? :redef :redef? :private :private? :macro :macro?})

(defn- parse-def [u env form]
  (check-count env form 1 3 "(def name doc? init?)")
  (let [[_ sym0 & more] form
        here (current-ns env)
        sym (if (and (qualified-symbol? sym0) (= (namespace sym0) (str here)))
              (with-meta (symbol (name sym0)) (meta sym0))
              sym0)
        _ (when-not (simple-symbol? sym)
            (fail env form :analyzer/bad-def
                  (str "def takes a simple symbol, or one qualified with " here
                       ", got " (pr-str sym0))))
        [doc init-form init?] (case (count more)
                                0 [nil nil false]
                                1 [nil (first more) true]
                                2 [(first more) (second more) true])
        _ (when (and (= 2 (count more)) (not (string? doc)))
            (fail env form :analyzer/wrong-arguments "def's doc string must be a string"))
        q (symbol (str here) (name sym))
        cenv (:cenv u)
        here-ns (env/find-ns cenv here)
        m (apply dissoc (meta sym) forms/reader-keys)
        info (cond-> (reduce-kv (fn [acc k k2] (if (get m k) (assoc acc k2 true) acc))
                                {:span (span-of env sym)} def-flags)
               (or doc (:doc m)) (assoc :doc (or doc (:doc m)))
               (:arglists m) (assoc :arglists (:arglists m))
               (:macro m) (assoc :macro-kind :replique))]
    (when (and (not= env/core-ns here)
               (env/find-var cenv (symbol (str env/core-ns) (name sym)))
               (not (contains? (:excludes here-ns) sym)))
      (swap! cenv update-in [:namespaces here :excludes] (fnil conj #{}) sym))
    ;; declared before the init is analysed, so the init can refer to it
    (env/define-var! cenv q (assoc info :defined? false))
    (when (and (not (:top-level? env)) (= :advanced (:mode u)))
      (warn! u env form :analyzer/inner-def
             (str q " is defined inside a function, so it is linked dynamically")))
    (let [init (when init?
                 (analyze u (at (assoc (not-tail env) :top-level? false) form (dec (count form)) init-form)
                          init-form))
          hinted (hint u env sym)
          fn? (= :fn (:op init))]
      (env/define-var! cenv q
                       (cond-> (assoc info :defined? init?)
                         fn? (assoc :arities (fn-arities init) :type (t/of :fn))
                         (and fn? hinted) (assoc :return-type hinted)
                         (and (not fn?) hinted) (assoc :type hinted)))
      (u/define! u q)
      (node env form :def t/nil-type
            (cond-> {:var q :top-level? (boolean (:top-level? env))}
              init (assoc :init init :children [:init]))))))

;; --- functions -------------------------------------------------------------

(defn- parse-method
  "One ([params] body...) of a fn*."
  [u env fn-form mform async?]
  (when-not (and (seq? mform) (vector? (first mform)))
    (fail env mform :analyzer/bad-fn
          (str "a fn method is a parameter vector and a body, got " (pr-str mform))))
  (let [env (enter env mform)
        params (first mform)
        amp (.indexOf ^java.util.List params '&)
        variadic? (not (neg? amp))
        _ (when (and variadic? (not= amp (- (count params) 2)))
            (fail env params :analyzer/bad-fn "& must be followed by exactly one parameter"))
        syms (if variadic? (conj (subvec params 0 amp) (peek params)) params)
        [nodes env'] (reduce (fn [[ns e] [i s]]
                               (let [[bn b] (binding-node u (at e params (if (and variadic? (= i amp)) (inc i) i) s)
                                                          s :arg nil)]
                                 [(conj ns bn) (with-local e b)]))
                             [[] env]
                             (map-indexed vector syms))
        id (u/next-id! u)
        b (body u (assoc env' :tail? true :top-level? false :async? async?
                         :recur {:id id :arity (count nodes)})
                mform mform 1)]
    (node env mform :fn-method (:type b)
          {:id id :params nodes
           :fixed-arity (if variadic? (dec (count nodes)) (count nodes))
           :variadic? variadic?
           :return-type (or (hint u env params) (:type b))
           :body b :children [:params :body]})))

(defn- user-meta [form]
  (some-> (meta form) (#(apply dissoc % forms/reader-keys)) not-empty))

(defn- wrap-meta
  "`n` wrapped in a :with-meta when `form` has metadata of its own: metadata
  on a literal is part of the value, and evaluated like a map literal."
  [u env form n]
  (if-let [m (user-meta form)]
    (let [mn (analyze u (not-tail env) m)]
      (node env form :with-meta (:type n) {:expr n :meta mn :children [:meta :expr]}))
    n))

(defn- parse-fn [u env form]
  (let [[_ & more] form
        fn-name (when (symbol? (first more)) (first more))
        more (if fn-name (rest more) more)
        methods (if (vector? (first more)) [(apply list more)] (vec more))
        async? (boolean (or (:async (meta (first form))) (:async (meta fn-name))))
        _ (when (empty? methods) (fail env form :analyzer/bad-fn "fn* needs at least one method"))
        [name-node name-b] (when fn-name (binding-node u env fn-name :fn (t/of :fn)))
        env' (cond-> (assoc env :top-level? false) name-b (with-local name-b))
        ;; a method written as ([x] ...) inside the fn* list has a span; one
        ;; the sugar made doesn't, and gets the fn's
        ms (mapv #(parse-method u env' form % async?) methods)
        fixed (remove :variadic? ms)
        variadic (filter :variadic? ms)]
    (when (> (count variadic) 1)
      (fail env form :analyzer/bad-fn "a fn can have only one variadic method"))
    (when-not (apply distinct? 0.5 (map :fixed-arity fixed))
      (fail env form :analyzer/bad-fn "two methods of a fn have the same arity"))
    (when (and (seq variadic) (some #(> (:fixed-arity %) (:fixed-arity (first variadic))) fixed))
      (fail env form :analyzer/bad-fn
            "a fixed arity can't have more parameters than the variadic method"))
    (wrap-meta u env form
               (node env form :fn (t/of :fn)
                     (cond-> {:methods (vec (concat (sort-by :fixed-arity fixed) variadic))
                              :variadic? (boolean (seq variadic))
                              :max-fixed-arity (apply max (map :fixed-arity ms))
                              :async? async?
                              :children [:methods]}
                       name-node (assoc :local name-node :children [:local :methods]))))))

(defn- parse-letfn [u env form]
  (let [bv (second form)]
    (binding-pairs env form bv "letfn*")
    (let [env (assoc env :top-level? false)
          pairs (mapv (fn [i] (binding-node u (at env bv i (nth bv i)) (nth bv i) :letfn (t/of :fn)))
                      (range 0 (count bv) 2))
          env' (reduce (fn [e [_ b]] (with-local e b)) env pairs)
          nodes (mapv (fn [[bn _] i]
                        (assoc bn :init (analyze u (at (not-tail env') bv (inc i) (nth bv (inc i)))
                                                 (nth bv (inc i)))
                               :children [:init]))
                      pairs (range 0 (count bv) 2))
          b (body u env' form form 2)]
      (node env form :letfn (:type b) {:bindings nodes :body b :children [:bindings :body]}))))

;; --- invocation ------------------------------------------------------------

(defn- check-arity! [u env form fnode argc]
  (when (= :var (:op fnode))
    (when-let [{:keys [fixed variadic]} (:arities (env/find-var (:cenv u) (:var fnode)))]
      (when-not (or (contains? fixed argc) (and variadic (>= argc variadic)))
        (warn! u env form :analyzer/wrong-arity
               (str (:var fnode) " is called with " argc " argument(s), and it takes "
                    (str/join ", " (concat (sort fixed) (when variadic [(str variadic "+")])))))))))

(defn- parse-invoke [u env form]
  (let [env' (not-tail env)
        f (sub u env' form 0)
        args (subs-from u env form 1)
        rt (when (= :var (:op f)) (:return-type (env/find-var (:cenv u) (:var f))))]
    (check-arity! u env form f (count args))
    (node env form :invoke (or rt t/any) {:fn f :args args :children [:fn :args]})))

;; --- set! ------------------------------------------------------------------

(defn- parse-set! [u env form]
  (check-count env form 2 3 "(set! target value) or (set! object -field value)")
  (let [form' (if (= 4 (count form))
                (with-meta (list 'set! (list '. (nth form 1) (nth form 2)) (nth form 3)) (meta form))
                form)
        target-form (nth form' 1)
        env' (not-tail env)
        target (sub u env' form' 1)]
    (case (:op target)
      :var (env/update-var! (:cenv u) (:var target) assoc :assigned? true)
      :host-field nil
      :local (when-not (:mutable? (get-in env [:locals target-form]))
               (fail env target-form :analyzer/bad-set!
                     (str "can't set! the local " target-form ": locals are immutable")))
      (fail env target-form :analyzer/bad-set!
            "set! takes a var, a host field or a mutable field"))
    (let [v (sub u env' form' 2)]
      (node env form :set! (:type v) {:target target :val v :children [:target :val]}))))

;; --- try -------------------------------------------------------------------

(defn- clause? [sym f] (and (seq? f) (= sym (first f))))

(defn- parse-catch [u env c]
  (when (< (count c) 3)
    (fail env c :analyzer/bad-try "(catch class name body...)"))
  (let [env (enter env c)
        [_ cls sym] c
        jvm? (= :jvm (host/target (:host u)))
        class-node (cond
                     (= :default cls) nil
                     jvm? (let [k (when (symbol? cls)
                                    (host/resolve-class (:host u) (imports u env) cls))]
                            (when-not (and k (.isAssignableFrom Throwable ^Class k))
                              (fail env cls :analyzer/bad-try
                                    (str (pr-str cls) " is not a Throwable class")))
                            (class-const env cls k))
                     :else (sub u (not-tail env) c 1))
        type (if (and jvm? class-node) (t/of (:val class-node)) t/any)
        [bn b] (binding-node u (at env c 2 sym) sym :catch type)
        bd (body u (with-local env b) c c 3)]
    (node env c :catch (:type bd)
          {:class class-node :local bn :body bd :children [:local :body]})))

(defn- parse-try
  "(try body* (catch class name body*)* (finally body*)?). A recur can't
  cross a try, in either direction."
  [u env form]
  (let [items (vec (rest form))
        n (count items)
        start (or (first (keep-indexed #(when (or (clause? 'catch %2) (clause? 'finally %2)) %1)
                                       items))
                  n)
        clauses (subvec items start)
        catches (vec (take-while #(clause? 'catch %) clauses))
        more (drop (count catches) clauses)
        fin (first more)]
    (when (or (> (count more) 1) (and fin (not (clause? 'finally fin))))
      (fail env form :analyzer/bad-try
            "a try is a body, then catch clauses, then at most one finally"))
    (let [defaults (keep-indexed #(when (= :default (second %2)) %1) catches)]
      (when (or (> (count defaults) 1)
                (and (seq defaults) (not= (first defaults) (dec (count catches)))))
        (fail env form :analyzer/bad-try
              "(catch :default ...) catches everything, so it comes last, and once")))
    (let [env' (assoc env :recur :barrier :top-level? false)
          b (body u (not-tail env') form form 1 (inc start))
          cs (mapv #(parse-catch u (not-tail env') %) catches)
          f (when fin (body u (not-tail env') fin fin 1))]
      (node env form :try (apply t/join (:type b) (map :type cs))
            (cond-> {:body b :catches cs :children [:body :catches]}
              f (assoc :finally f :children [:body :catches :finally]))))))

;; --- case* -----------------------------------------------------------------

(defn- case-label
  "What a test is compared as: in JS, 1 and 1.0 are one number, and a
  character is a one-character string."
  [target v]
  (if (= :js target)
    (cond (and (number? v) (not (instance? clojure.lang.BigInt v))) (double v)
          (char? v) (str v)
          :else v)
    v))

(defn- parse-case
  "(case* expr [[test ...] ...] [then ...] default?): tests grouped per
  branch, constants never evaluated. Without a default, no match throws."
  [u env form]
  (check-count env form 3 4 "(case* expr [[tests] ...] [thens] default?)")
  (let [[_ _ tests thens] form
        target (host/target (:host u))]
    (when-not (and (vector? tests) (every? vector? tests) (every? seq tests)
                   (vector? thens) (= (count tests) (count thens)))
      (fail env form :analyzer/bad-case
            "case* takes a vector of non-empty test vectors and a vector of as many branches"))
    (let [labels (map #(case-label target (program-value %)) (apply concat tests))]
      (when-not (or (empty? labels) (apply distinct? labels))
        (fail env form :analyzer/bad-case
              (str "two case tests are the same" (when (= :js target) " in JS")))))
    (let [test (sub u (not-tail env) form 1)
          nodes (mapv (fn [group i]
                        (let [then (sub u env thens i)]
                          (node (at env tests i group) group :case-node (:type then)
                                {:tests (mapv (fn [j]
                                                (let [x (nth group j)]
                                                  (const u (at env group j x) x (program-value x))))
                                              (range (count group)))
                                 :then then :children [:tests :then]})))
                      tests (range))
          default (when (= 5 (count form)) (sub u env form 4))]
      (node env form :case (apply t/join (cond-> (map :type nodes) (nil? default) (conj t/never)
                                           default (conj (:type default))))
            (cond-> {:test test :nodes nodes :children [:test :nodes]}
              default (assoc :default default :children [:test :nodes :default]))))))

;; --- host interop ----------------------------------------------------------

(defn- jvm? [u] (= :jvm (host/target (:host u))))

(defn- reflective! [u env form what]
  (if (= :advanced (:mode u))
    (fail env form :analyzer/reflection (str what " needs reflection, which advanced mode doesn't allow"))
    (warn! u env form :analyzer/reflection (str what " needs reflection"))))

(defn- class-const [env form ^Class c]
  (node (enter env form) form :const (t/of Class) {:val c :kind :class}))

(defn- static-class
  "The class a symbol in a target or class position names on the JVM, when
  it isn't a local or a var."
  [u env sym]
  (when (and (jvm? u) (symbol? sym) (nil? (namespace sym))
             (not (get-in env [:locals sym]))
             (not (env/resolve-var (:cenv u) (current-ns env) sym)))
    (host/resolve-class (:host u) (imports u env) sym)))

(defn- target-class
  "The class of the values of type `t`, if known."
  [t]
  (let [b (:base t)]
    (cond (class? b) b
          (t/primitive? t) (:base (t/nilable t))
          :else nil)))

(defn- host-access
  "A field read or a method call. `target` is a node, `cls` the class a
  static access is on."
  [u env form target cls static? member args]
  (let [mname (name member)
        field? (str/starts-with? mname "-")
        mname (if field? (subs mname 1) mname)
        msym (symbol mname)
        k (or cls (when (jvm? u) (target-class (:type target))))
        what (str (if static? (str (.getName ^Class cls) "/") ".") mname)]
    (when (and field? (seq args))
      (fail env form :analyzer/bad-interop (str "a field read takes no arguments: " what)))
    (let [h (:host u)
          m (when-not field? (host/method h k msym (arg-types args) static?))
          f (when (or field? (and (empty? args) (:none? m)))
              (host/field h k msym static?))
          {:keys [member type none?] :as r} (or f m)]
      (when (and (jvm? u) k none?)
        (fail env form :analyzer/no-such-member
              (str "no " (cond field? "field" (empty? args) "method or field" :else "method")
                   " " mname
                   (when-not field? (str " taking " (count args) " argument(s)"))
                   " in " (.getName ^Class k))))
      (when (= :reflective member) (reflective! u env form what))
      (if f
        (node env form :host-field type
              (cond-> {:target target :field msym :static? (boolean static?) :children [:target]}
                (jvm? u) (assoc :jvm/member member)))
        (node env form :host-call type
              (cond-> {:target target :method msym :args args :static? (boolean static?)
                       :super? false :children [:target :args]}
                (jvm? u) (assoc :jvm/member member)))))))

(defn- parse-dot
  "(. target member args*), (. target (member args*)), (. target -field)."
  [u env form]
  (when (< (count form) 3)
    (fail env form :analyzer/bad-interop "(. target member args*)"))
  (let [[_ tform mform] form
        env' (not-tail env)
        [member args] (if (seq? mform)
                        [(first mform) (subs-from u env' (vec mform) 1)]
                        [mform (subs-from u env' (vec form) 3)])
        _ (when-not (simple-symbol? member)
            (fail env form :analyzer/bad-interop (str "a member is a symbol, got " (pr-str member))))
        cls (static-class u env tform)
        target (if cls (class-const (at env form 1 tform) tform cls) (sub u env' form 1))]
    (host-access u env form target cls (boolean cls) member args)))

(defn- parse-new [u env form]
  (when (< (count form) 2)
    (fail env form :analyzer/bad-interop "(new Class args*)"))
  (let [cform (second form)
        env' (not-tail env)
        args (subs-from u env' (vec form) 2)]
    (if (jvm? u)
      (let [c (when (symbol? cform) (host/resolve-class (:host u) (imports u env) cform))
            _ (when-not c (fail env cform :analyzer/unknown-class (str "no class " (pr-str cform))))
            {:keys [member type]} (host/constructor (:host u) c (arg-types args))]
        (when (= :reflective member) (reflective! u env form (str "new " (.getName ^Class c))))
        (node env form :new type {:class (class-const (at env form 1 cform) cform c) :args args
                                  :jvm/member member :children [:class :args]}))
      (node env form :new t/any {:class (sub u env' form 1) :args args :children [:class :args]}))))

(defn- qualified-method [u env sym ^Class c]
  (let [n (name sym)
        [kind mname] (cond (= "new" n) [:ctor nil]
                           (str/starts-with? n ".") [:instance (symbol (subs n 1))]
                           :else [:static (symbol n)])
        ms (host/members (:host u) c mname kind)]
    (when (empty? ms)
      (fail env sym :analyzer/no-such-member (str "no " (name kind) " method " sym)))
    (node env sym :qualified-method (t/of :fn)
          {:class (class-const env sym c) :kind kind :method mname :jvm/members ms})))

(defn- static-member-value
  "Class/name as a value: a static field, or a method as a function."
  [u env sym ^Class c]
  (let [n (name sym)
        {:keys [none?] :as f} (when-not (or (= "new" n) (str/starts-with? n "."))
                                (host/field (:host u) c (symbol n) true))]
    (if (and f (not none?))
      (node env sym :host-field (:type f)
            {:target (class-const env sym c) :field (symbol n) :static? true
             :jvm/member (:member f) :children [:target]})
      (qualified-method u env sym c))))

(defn- interop-sugar
  "The form `form` stands for when its head is interop sugar, or nil:
  (.m o args), (.-f o), (Class. args), (Class/m args), (Class/.m o args),
  (Class/new args); in JS, (js/Ctor. args)."
  [u env form]
  (let [op (first form)
        n (name op)
        m (meta form)]
    (cond
      (and (namespace op) (not= "js" (namespace op)))
      (when-let [c (and (jvm? u)
                        (not (env/resolve-ns (:cenv u) (current-ns env) (symbol (namespace op))))
                        (host/resolve-class (:host u) (imports u env) (symbol (namespace op))))]
        (let [args (subs-from u (not-tail env) (vec form) 1)]
          (cond
            (= "new" n)
            (let [{:keys [member type]} (host/constructor (:host u) c (arg-types args))]
              (when (= :reflective member) (reflective! u env form (str "new " (.getName ^Class c))))
              (node env form :new type {:class (class-const env op c) :args args
                                        :jvm/member member :children [:class :args]}))
            (str/starts-with? n ".")
            (do (when (empty? args)
                  (fail env form :analyzer/bad-interop (str op " needs the object to call it on")))
                (host-access u env form (first args) c false (symbol (subs n 1)) (vec (rest args))))
            :else
            (host-access u env form (class-const env op c) c true (symbol n) args))))

      (and (str/starts-with? n ".") (> (count n) 1) (not= ".." n))
      (do (when (< (count form) 2)
            (fail env form :analyzer/bad-interop (str "(" op " object args*)")))
          (let [env' (not-tail env)
                target (sub u env' form 1)
                cls (static-class u env (second form))
                target (if cls (class-const (at env form 1 (second form)) (second form) cls) target)]
            (host-access u env form target cls (boolean cls) (symbol (subs n 1))
                         (subs-from u env' (vec form) 2))))

      (and (str/ends-with? n ".") (> (count n) 1))
      (parse-new u env (with-meta (list* 'new (symbol (namespace op) (subs n 0 (dec (count n)))) (rest form)) m)))))

;; --- JS --------------------------------------------------------------------

(defn- js-only [u env form what]
  (when (jvm? u)
    (fail env form :analyzer/js-only (str what " is only valid in code compiled for JS"))))

(defn- parse-js* [u env form]
  (js-only u env form "js*")
  (let [[_ tmpl] form]
    (when-not (string? tmpl)
      (fail env form :analyzer/bad-js* "js* takes a string template"))
    (let [segs (vec (.split ^String tmpl "~\\{\\}" -1))
          args (subs-from u env (vec form) 2)]
      (when-not (= (count args) (dec (count segs)))
        (fail env form :analyzer/bad-js*
              (str "the js* template has " (dec (count segs)) " hole(s) and "
                   (count args) " argument(s)")))
      (node env form :js/inline t/any {:template segs :args args :children [:args]}))))

(defn- analyze-js-literal [u env form]
  (js-only u env form "#js")
  (let [x (:form form)
        env' (not-tail env)]
    (cond
      (map? x)
      (node env form :js/object t/any
            {:keys (mapv (fn [k]
                           (cond (keyword? k) (if (namespace k) (str (namespace k) "/" (name k)) (name k))
                                 (string? k) k
                                 :else (fail env form :analyzer/bad-js
                                             "#js {} keys are keywords or strings")))
                         (keys x))
             :vals (mapv (fn [[k v]] (analyze u (at env' x k v) v)) x)
             :children [:vals]})
      (vector? x)
      (node env form :js/array t/any
            {:items (mapv #(sub u env' x %) (range (count x))) :children [:items]})
      :else (fail env form :analyzer/bad-js "#js takes a map or a vector"))))

;; --- locking and await -----------------------------------------------------

(defn- parse-locking [u env form]
  (check-count env form 1 nil "(locking lock body*)")
  (let [lock (sub u (not-tail env) form 1)
        b (body u (assoc env :recur :barrier :top-level? false :tail? false) form form 2)]
    (node env form :locking (:type b) {:lock lock :body b :children [:lock :body]})))

(defn- parse-await [u env form]
  (check-count env form 1 1 "(await expr)")
  (when-not (:async? env)
    (fail env form :analyzer/await-outside-async "await is only valid inside an ^:async fn"))
  (let [e (sub u (not-tail env) form 1)]
    (node env form :await t/any {:expr e :children [:expr]})))

;; --- ns --------------------------------------------------------------------

(defn- ns-error [env form message]
  (fail env form :analyzer/bad-ns message))

(defn- parse-require [u env clause decl spec]
  (let [cenv (:cenv u)
        [lib & opts] (if (vector? spec) spec [spec])
        _ (when-not (simple-symbol? lib)
            (ns-error env clause (str "a require names a namespace, got " (pr-str spec))))
        _ (when-not (even? (count opts))
            (ns-error env clause (str "a require's options come in pairs: " (pr-str spec))))
        {:keys [as as-alias] :as o} (apply hash-map opts)]
    (when (contains? o :refer)
      (ns-error env clause (str ":refer isn't supported: name the vars through an alias, "
                                "(:require [" lib " :as ...])")))
    (when-let [bad (seq (remove #{:as :as-alias} (keys o)))]
      (ns-error env clause (str "unknown require options " (vec bad))))
    (when (and (not as-alias) (not (env/find-ns cenv lib)))
      (fail env clause :analyzer/unknown-namespace (str "the namespace " lib " isn't loaded")))
    (cond-> decl
      (not as-alias) (update :requires conj lib)
      as (assoc-in [:aliases as] lib)
      as-alias (assoc-in [:aliases as-alias] lib))))

(defn- parse-import [u env clause decl spec]
  (let [[prefix & names] (cond (symbol? spec) (let [s (str spec) i (.lastIndexOf s ".")]
                                                [(symbol (subs s 0 (max i 0))) (symbol (subs s (inc i)))])
                               (sequential? spec) spec
                               :else (ns-error env clause (str "bad import " (pr-str spec))))]
    (reduce (fn [d n]
              (let [full (symbol (str prefix "." n))]
                (when-not (host/resolve-class (:host u) {} full)
                  (fail env clause :analyzer/unknown-class (str "no class " full)))
                (assoc-in d [:imports n] full)))
            decl names)))

(defn- parse-ns
  "(ns name doc? options? (:require ...)* (:import ...)* (:refer-clojure ...)?)
  Required namespaces must be loaded already: loading them is the driver's
  job, and ns-requires says which they are."
  [u env form]
  (when-not (:top-level? env)
    (fail env form :analyzer/bad-ns "ns is only valid at the top level"))
  (let [[_ nm & more] form
        _ (when-not (simple-symbol? nm) (ns-error env form "ns takes a simple symbol"))
        more (if (string? (first more)) (rest more) more)
        [options more] (if (map? (first more)) [(program-value (first more)) (rest more)] [{} more])
        decl (reduce
              (fn [decl clause]
                (when-not (and (seq? clause) (keyword? (first clause)))
                  (ns-error env clause (str "an ns clause is a list starting with a keyword, got "
                                            (pr-str clause))))
                (case (first clause)
                  :require (reduce #(parse-require u env clause %1 %2) decl (rest clause))
                  :import (if (jvm? u)
                            (reduce #(parse-import u env clause %1 %2) decl (rest clause))
                            (ns-error env clause ":import is for JVM classes"))
                  :refer-clojure
                  (let [{:keys [exclude only]} (apply hash-map (rest clause))]
                    (cond-> decl
                      exclude (update :excludes into exclude)
                      only (update :excludes into
                                   (remove (set only)
                                           (keys (:vars (env/find-ns (:cenv u) env/core-ns)))))))
                  (ns-error env clause (str "unknown ns clause " (first clause)))))
              {:aliases {} :imports {} :excludes #{} :requires #{} :options options}
              more)]
    (env/declare-ns! (:cenv u) nm decl)
    (vreset! (:ns-after u) nm)
    (node env form :ns t/nil-type
          {:name nm :requires (vec (sort (:requires decl))) :options options})))

(defn ns-requires
  "The namespaces an ns form requires, for the driver to load first."
  [form]
  (when (and (seq? form) (= 'ns (first form)))
    (vec (for [clause (drop 2 form)
               :when (and (seq? clause) (= :require (first clause)))
               spec (rest clause)
               :let [[lib & opts] (if (vector? spec) spec [spec])]
               :when (not (:as-alias (apply hash-map opts)))]
           lib))))

;; --- symbols ---------------------------------------------------------------

(defn- analyze-symbol [u env sym]
  (let [ns-part (namespace sym)]
    (if-let [b (and (nil? ns-part) (get-in env [:locals sym]))]
      (local-node env sym b)
      (if-let [v (var-ref! u env sym)]
        (var-node u env sym v)
        (cond
          (= "js" ns-part)
          (do (js-only u env sym "js/")
              (node env sym :js/global t/any {:name (name sym)}))

          (and ns-part (jvm? u))
          (if-let [c (host/resolve-class (:host u) (imports u env) (symbol ns-part))]
            (static-member-value u env sym c)
            (fail env sym :analyzer/unresolved-symbol
                  (str sym " names no var" (when-not (env/resolve-ns (:cenv u) (current-ns env) (symbol ns-part))
                                             (str ": " ns-part " is neither a namespace nor a class")))))

          :else
          (if-let [c (and (jvm? u) (host/resolve-class (:host u) (imports u env) sym))]
            (class-const env sym c)
            (fail env sym :analyzer/unresolved-symbol
                  (str sym " is neither a local nor a var in " (current-ns env)))))))))

;; --- collections -----------------------------------------------------------

(defn- analyze-vector [u env form]
  (wrap-meta u env form
             (node env form :vector (t/of :vector)
                   {:items (mapv #(sub u env form %) (range (count form))) :children [:items]})))

(defn- analyze-set [u env form]
  (wrap-meta u env form
             (node env form :set (t/of :set)
                   {:items (mapv #(analyze u (at env form % %) %) form) :children [:items]})))

(defn- analyze-map [u env form]
  (wrap-meta u env form
             (node env form :map (t/of :map)
                   {:keys (mapv #(analyze u (at-key env form %) %) (keys form))
                    :vals (mapv (fn [[k v]] (analyze u (at env form k v) v)) form)
                    :children [:keys :vals]})))

;; --- dispatch --------------------------------------------------------------

(def ^:private not-yet
  '#{deftype* reify* definterface})

(defn- parse-clause-outside [u env form]
  (if (get-in env [:locals (first form)])
    (parse-invoke u env form)
    (fail env form :analyzer/bad-try (str (first form) " is only valid inside a try"))))

(def ^:private parsers
  {'if parse-if 'do parse-do 'let* parse-let 'loop* parse-loop 'recur parse-recur
   'fn* parse-fn 'letfn* parse-letfn 'def parse-def 'quote parse-quote 'var parse-var
   'throw parse-throw 'try parse-try 'set! parse-set! 'case* parse-case
   '. parse-dot 'new parse-new 'ns parse-ns 'js* parse-js* 'locking parse-locking
   'await parse-await})

(defn- macro-var [u env sym]
  (when-not (and (nil? (namespace sym)) (get-in env [:locals sym]))
    (let [v (env/resolve-var (:cenv u) (current-ns env) sym)]
      (when (:macro? v) v))))

(defn- analyze-seq [u env form]
  (let [op (first form)]
    (cond
      (not (symbol? op)) (parse-invoke u env form)
      (parsers op) ((parsers op) u env form)
      (not-yet op) (fail env form :analyzer/not-implemented (str op " arrives with core (M2)"))
      (#{'catch 'finally} op) (parse-clause-outside u env form)
      :else
      (if-let [info (macro-var u env op)]
        (let [{:keys [pos]} env
              {expansion :form id :id} (mx/expand u env form info (:span pos) (:origin pos))]
          (analyze u (assoc env :pos {:span (:span pos) :origin id}) expansion))
        (or (when-not (get-in env [:locals op]) (interop-sugar u env form))
            (parse-invoke u env form))))))

(defn analyze
  "The IR node for `form` in `env`, within the unit `u`."
  [u env form]
  (let [env (enter env form)]
    (cond
      (seq? form) (if (seq form) (analyze-seq u env form) (const u env form ()))
      (symbol? form) (analyze-symbol u env form)
      (vector? form) (analyze-vector u env form)
      (map? form) (analyze-map u env form)
      (set? form) (analyze-set u env form)
      (and (instance? TaggedLiteral form) (= 'js (:tag form))) (analyze-js-literal u env form)
      :else (const u env form form))))

;; --- the top level ---------------------------------------------------------

(defn environment
  "A new compiler environment holding replique.core's kernel declarations
  and bootstrap macros."
  []
  (bootstrap/install! (env/create)))

(defn- top-env [ns]
  {:ns ns :locals {} :tail? false :recur nil :top-level? true :async? false
   :pos {:span nil :origin nil}})

(defn analyze-top
  "Analyse one top-level form in namespace `ns`. Returns the record of
  doc/ir.md §1:

    :node         the root node, or nil if the form has an error
    :expansions   the expansion table, {id {:macro :call :parent}}
    :inputs       {:vars :macros :files :env}, what the form consumed
    :defines      the vars it defines
    :diagnostics  its warnings and error
    :ns           the current namespace after it

  Options: :ns (default user), :host (default the JVM's), :mode (:dev or
  :advanced), :file, :gensym-counter."
  [cenv form {:keys [ns host] :as opts}]
  (let [ns (or ns 'user)
        host (or host (host/jvm))
        u (assoc (u/create (assoc opts :cenv cenv :host host :ns ns))
                 :ns-after (volatile! nil))
        record (fn [n error]
                 {:node n
                  :expansions @(:expansions u)
                  :inputs @(:inputs u)
                  :defines @(:defines u)
                  :diagnostics (cond-> @(:warnings u) error (conj error))
                  :ns (or @(:ns-after u) ns)})]
    (env/ensure-ns! cenv ns)
    (try
      (record (passes/run (analyze u (top-env ns) form)) nil)
      (catch clojure.lang.ExceptionInfo e
        (if-let [d (u/diagnostic e)]
          (record nil d)
          (throw e))))))

(defn syntax-quote-resolver
  "How syntax-quote qualifies symbols, given what the environment knows:
  aliases, vars, imported classes."
  [cenv host]
  (fn [{:keys [ns] :as ctx} sym]
    (cond
      (namespace sym)
      (if-let [full (env/resolve-ns cenv ns (symbol (namespace sym)))]
        (symbol (str full) (name sym))
        sym)

      :else
      (if-let [v (env/resolve-var cenv ns sym)]
        (:var v)
        (if-let [c (and (= :jvm (host/target host))
                        (host/resolve-class host (:imports (env/find-ns cenv ns)) sym))]
          (symbol (.getName ^Class c))
          (forms/default-resolve-symbol ctx sym))))))

(defn analyze-source
  "Read and analyse every form of `src`, in order: an ns form changes the
  namespace, the aliases and the syntax-quote resolution of the forms after
  it. Returns {:units [record ...] :diagnostics reader-diagnostics
  :incomplete? b}.

  Options: :file, :ns (default user), :target (:jvm or :js), :host, :mode,
  and :on-unit, called with each record before the next form is read: a
  driver runs the form there, so that the forms after it see what it
  defined."
  [cenv src {:keys [file target host on-unit] :as opts}]
  (let [host (or host (if (= :js target) (host/js) (host/jvm)))
        target (host/target host)
        p (cst/parse src {:file file})
        counter (atom 0)
        resolver (syntax-quote-resolver cenv host)
        with-span (fn [d] (-> d (dissoc :start :end) (assoc :span (cst/span p (:start d) (:end d)))))]
    (loop [nodes (:nodes p) ns (or (:ns opts) 'user) units [] diags (mapv with-span (:diagnostics p))]
      (if-let [n (first nodes)]
        (let [ctx (forms/context p {:target target :ns ns
                                    :aliases (:aliases (env/find-ns cenv ns))
                                    :resolve-symbol resolver :gensym-counter counter})
              [fs ds] (forms/top-level ctx n)
              [ns units] (reduce (fn [[ns units] f]
                                   (let [r (analyze-top cenv f (-> opts
                                                                   (dissoc :on-unit)
                                                                   (assoc :ns ns :host host
                                                                          :gensym-counter counter)))]
                                     (when on-unit (on-unit r))
                                     [(:ns r) (conj units r)]))
                                 [ns units] fs)]
          (recur (rest nodes) ns units (into diags (map with-span) ds)))
        {:units units :diagnostics diags :incomplete? (cst/incomplete? p)}))))
