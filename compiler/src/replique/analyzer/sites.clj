(ns replique.analyzer.sites
  "Code compiled to be recorded (doc/compilation.md §6, doc/recording.md):
  the pass that chooses the sites of a top-level form, which both back ends
  then emit the hook's calls for.

    (mark record {:site f :locals? true})

  `f` is called at compile time with what each site is, and answers the
  number the code passes the hook, or nil for no site there:

    {:kind :value  :span s :op op}               a form the source wrote
    {:kind :enter  :span s :outermost b :params [x & xs]}   a fn method
    {:kind :local  :span s :name x}              a let or loop name, with
                                                 :locals? only

  A value site whose node a macro built has the span of the macro call and
  its :macro. Sites are asked for in the order of the tree, children first:
  a method's body before the method, a binding's init before the binding.
  The number lands on the node as :site: on the expression for a value
  site, on the :fn-method for an enter site, on the :binding for a local.

  What is a site, and what isn't, is doc/recording.md §2."
  (:require [replique.ir :as ir]))

(def ^:private unreported
  "The ops whose value isn't reported though the source wrote them: a fn
  reports entering instead, a definition defines rather than computes, a
  constant is known, and recur and throw never have a value. And the nodes
  that aren't expressions."
  #{:fn :def :ns :the-var :const :quote :recur :throw :jvm/definterface :deftype
    :binding :fn-method :case-node :catch :method})

(defn- chain
  "The expansions `origin` is in, innermost first."
  [expansions origin]
  (take-while some? (iterate #(:parent (get expansions %)) origin)))

(defn- top-expansion
  "The outermost expansion of `origin`'s chain: the one whose call the
  source wrote."
  [expansions origin]
  (last (chain expansions origin)))

(defn- defines?
  "Whether `node` defines rather than computes: a def, or a do a macro made
  of definitions, as declare does."
  [node]
  (or (contains? #{:def :ns :deftype :jvm/definterface} (:op node))
      (and (= :do (:op node))
           (some? (:origin node))
           (boolean (some defines? (conj (:statements node) (:ret node)))))))

(defn- never? [{{:keys [base nil?]} :type}]
  (and (= :never base) (not nil?)))

(defn- value-place
  "Where `node`, whose parent is `parent`, reports its value: {:span}, with
  the :macro of the call when a macro built it, or nil when it isn't a site.

  A node the source wrote is one, when it is a list: not a symbol, not a
  literal. A node a macro built is one when it is the root of an expansion
  whose call the source wrote, so a call reports at the call, and a macro
  that hands back a form the source wrote reports nothing of its own. A
  node standing for the same form as its parent, the implicit do of a body,
  is its parent's."
  [expansions node parent]
  (when-not (or (contains? unreported (:op node))
                (never? node)
                (and (= :with-meta (:op node)) (= :fn (:op (:expr node))))
                (and parent (identical? (:form node) (:form parent))))
    (if-let [o (:origin node)]
      (let [top (top-expansion expansions o)]
        (when-not (and parent (some #{top} (chain expansions (:origin parent))))
          (when-not (defines? node)
            (let [{:keys [call macro]} (get expansions top)]
              {:span call :macro macro}))))
      (when (and (seq? (:form node)) (seq (:form node)))
        {:span (:span node)}))))

(defn- written-params
  "The parameters of `method` as the source wrote them, an & before the
  rest one."
  [{:keys [params variadic?]}]
  (let [names (mapv :name params)]
    (if (and variadic? (seq names))
      (conj (pop names) '& (peek names))
      names)))

(defn- written-method?
  "Whether the fn method `method` is the source's, rather than one a macro
  wrapped round it (a dispatcher over arities, say): written by hand, with
  a site inside it, with no parameter, or with a parameter the source wrote.
  One function of the source is entered once."
  [method sites?]
  (or (nil? (:origin method))
      sites?
      (empty? (:params method))
      (boolean (some #(nil? (:origin %)) (:params method)))))

(defn mark
  "The record of a top-level form with its sites marked. Options:

    :site     the function choosing each site, its number or nil
    :locals?  whether the names a let or a loop binds are sites too"
  [{:keys [node expansions] :as record} {:keys [site locals?]}]
  (let [counted (volatile! 0)
        ask (fn [m] (let [n (site m)] (when n (vswap! counted inc)) n))
        ;; in-fn?: whether a fn method encloses `n`
        walk (fn walk [n parent in-fn?]
               (let [before @counted
                     n (ir/update-children n #(walk % n (or in-fn? (= :fn-method (:op n)))))]
                 (cond
                   (= :fn-method (:op n))
                   (if-let [s (when (written-method? n (> @counted before))
                                (ask {:kind :enter :span (:span n) :outermost (not in-fn?)
                                      :params (written-params n)}))]
                     (assoc n :site s)
                     n)

                   (= :binding (:op n))
                   (if-let [s (when (and locals? (#{:let :loop} (:local n)) (nil? (:origin n)))
                                (ask {:kind :local :span (:span n) :name (:name n)}))]
                     (assoc n :site s)
                     n)

                   :else
                   (if-let [s (some-> (value-place expansions n parent)
                                      (assoc :kind :value :op (:op n))
                                      ask)]
                     (assoc n :site s)
                     n))))]
    (assoc record :node (when node (walk node nil false)))))
