(ns replique.js.emit
  "The JS back end: the IR of doc/ir.md to JavaScript source, dev mode
  (doc/compilation.md §2.2). Pure: it returns text, and replique.js.dev
  runs it.

  A top-level form compiles to a function of the runtime, which the driver
  calls once:

    (function ($rt) {
    \"use strict\";
    const app$core$ns = $rt.namespace(\"app.core\");   one per namespace used
    const c$1 = $rt.keyword(null, \"k\", -1234);       keywords and symbols
    ...
    return <the value>;
    })

  A var is a property of its namespace object, named by munge
  (replique.js.names), so the REPL can redefine anything.

  Emission is destination-driven, as in replique-cljs's emitter
  (doc/cljs-compiler.md §5.1 there): emitting a node returns

    :stmts       statements that must run first
    :expr        an expression holding the value
    :terminal?   control never leaves (a throw, a recur); :expr is nil

  so an if, a let, a try or a case in an argument is statements in the
  enclosing function and a temporary, never a function wrapper. Two more
  flags: :stable? (a constant or a local, which a later sibling's
  statements can't change, so in-order needn't pin it in a temporary) and
  :no-effect? (dropped when used as a statement).

  Scopes flatten into the function, so every local is named uniquely within
  the form (`local-name`). A loop that is recurred to keeps two names per
  binding: a carrier that recur assigns, and the binding's own name,
  declared from it at the top of each iteration, so a closure made in one
  iteration keeps that iteration's values.

  A form compiled to be recorded (doc/recording.md), its sites marked by
  replique.analyzer.sites, is a function of the runtime and the hook,
  `(function ($rt, $hook) ...)`, and reports to $hook where the sites are."
  (:require [clojure.string :as str]
            [replique.analyzer.unit :as u]
            [replique.ir :as ir]
            [replique.js.names :as names])
  (:import [clojure.lang BigInt Keyword Symbol]
           [java.math BigInteger]
           [java.time Instant]
           [java.util.regex Pattern]))

;; --- errors ----------------------------------------------------------------

(defn- unsupported [node what]
  (u/fail (:span node) :js/not-implemented (str what " isn't compiled for JS yet")))

;; --- text ------------------------------------------------------------------

(defn string-literal
  "`s` as a JS string literal, which is also a JSON string: everything but
  printable ASCII escaped, so a lone surrogate survives."
  [s]
  (let [sb (StringBuilder. "\"")]
    (doseq [c (str s)]
      (case c
        \" (.append sb "\\\"")
        \\ (.append sb "\\\\")
        \newline (.append sb "\\n")
        \tab (.append sb "\\t")
        \return (.append sb "\\r")
        (if (<= 0x20 (int c) 0x7e)
          (.append sb c)
          (.append sb (format "\\u%04x" (int c))))))
    (str (.append sb "\""))))

(defn- indent [lines] (mapv #(str "  " %) lines))

(defn- comma [xs] (str/join ", " xs))

;; --- emission state --------------------------------------------------------

;; ctx, for the form being emitted:
;;   :ns       the namespace
;;   :n        a volatile counter, numbering locals and temporaries
;;   :names    a volatile {binding-id js-name}
;;   :consts   a volatile [[name init] ...], made once at the top of the form
;;   :const-ids a volatile {key name}
;;   :aliases  a volatile {ns alias}
;;   :recur    {target-id [carrier ...]}
;;   :hook?    whether the form is compiled to be recorded
;;   :call     under recording, the JS name of the current call, or "null"

(defn- next-n! [ctx] (vswap! (:n ctx) inc))

(defn- temp! [ctx] (str "t$" (next-n! ctx)))

(defn- local! [ctx binding]
  (let [n (names/local-name (:name binding) (next-n! ctx))]
    (vswap! (:names ctx) assoc (:id binding) n)
    n))

(defn- local-ref [ctx id]
  (or (get @(:names ctx) id)
      (throw (IllegalStateException. (str "no name for local " id)))))

(defn- alias! [ctx ns]
  (or (get @(:aliases ctx) ns)
      (let [base (names/ns-alias ns)
            taken (set (vals @(:aliases ctx)))
            a (if (taken base) (str (subs base 0 (- (count base) 3)) "$" (next-n! ctx) "$ns") base)]
        (vswap! (:aliases ctx) assoc ns a)
        a)))

(defn- const! [ctx k init]
  (or (get @(:const-ids ctx) k)
      (let [c (str "c$" (next-n! ctx))]
        (vswap! (:const-ids ctx) assoc k c)
        (vswap! (:consts ctx) conj [c init])
        c)))

;; --- results ---------------------------------------------------------------

(defn- result [stmts expr] {:stmts (vec stmts) :expr expr})

(defn- terminal [r] (assoc r :terminal? true :expr nil))

(defn- stable [r] (assoc r :stable? true))

(defn- no-effect [r] (assoc r :no-effect? true))

(defn- pure [expr] (-> (result [] expr) stable no-effect))

(defn- effect-stmts
  "A result's statements, and its value evaluated for its effects unless it
  has none."
  [{:keys [stmts expr no-effect? terminal?]}]
  (cond-> (vec stmts)
    (not (or no-effect? terminal?)) (conj (str expr ";"))))

(defn- short-circuit
  "When one of `results`, which feed one node in order, never returns, the
  node never gets its value: what runs is the results before it, and it."
  [results]
  (when (some :terminal? results)
    (let [kept (vec (take-while (complement :terminal?) results))
          cut (nth results (count kept))]
      (terminal (result (-> (into [] (mapcat effect-stmts) kept) (into (:stmts cut))) nil)))))

(defn- in-order
  "Statements and expressions for `results`, keeping left-to-right order: a
  result that isn't stable is pinned in a temporary as soon as a later one
  has statements. `spill-all?` pins every one, for recur's simultaneous
  rebinding."
  ([ctx results] (in-order ctx results false))
  ([ctx results spill-all?]
   (let [v (vec results)]
     (loop [i 0 stmts [] exprs []]
       (if (= i (count v))
         {:stmts stmts :exprs exprs}
         (let [{:keys [expr stable?] :as r} (nth v i)
               stmts (into stmts (:stmts r))]
           (if (or spill-all? (and (not stable?) (some #(seq (:stmts %)) (subvec v (inc i)))))
             (let [tmp (temp! ctx)]
               (recur (inc i) (conj stmts (str "const " tmp " = " expr ";")) (conj exprs tmp)))
             (recur (inc i) stmts (conj exprs expr)))))))))

(declare emit emit-fn)

(defn- report
  "The statement handing the hook what the local site `b` is bound to,
  under recording, or nil."
  [ctx b]
  (when-let [site (and (:hook? ctx) (:site b))]
    (str "$hook.value(" site ", " (local-ref ctx (:id b)) ", " (:call ctx) ");")))

(defn- emit-all
  "Emit `nodes`, whose values build one expression with `build`, called with
  their expressions."
  [ctx nodes build]
  (let [rs (mapv #(emit ctx %) nodes)]
    (or (short-circuit rs)
        (let [{:keys [stmts exprs]} (in-order ctx rs)]
          (result stmts (build exprs))))))

(defn- body-lines
  "A function body: its statements, then the return."
  [ctx node]
  (let [{:keys [stmts expr terminal?]} (emit ctx node)]
    (cond-> (vec stmts)
      (not terminal?) (conj (str "return " expr ";")))))

(defn- assign-lines
  "The statements of `r`, then its value assigned to `v`."
  [r v]
  (cond-> (vec (:stmts r))
    (not (:terminal? r)) (conj (str v " = " (:expr r) ";"))))

;; --- constants -------------------------------------------------------------

(defn- number-js [v]
  (cond
    (or (instance? BigInt v) (instance? BigInteger v)) (str v "n")
    (integer? v) (if (neg? v) (str "(" v ")") (str v))
    :else (let [d (double v)]
            (cond
              (Double/isNaN d) "NaN"
              (Double/isInfinite d) (if (pos? d) "Infinity" "(-Infinity)")
              (or (neg? d) (= -0.0 d) (= "-0.0" (str d))) (str "(" d ")")
              :else (str d)))))

(defn- keyword-js [ctx ^Keyword k]
  (const! ctx [:keyword k]
          (str "$rt.keyword(" (if-let [n (namespace k)] (string-literal n) "null") ", "
               (string-literal (name k)) ", " (hash k) ")")))

(defn- symbol-js [ctx ^Symbol s]
  (const! ctx [:symbol (namespace s) (name s)]
          (str "$rt.symbol(" (if-let [n (namespace s)] (string-literal n) "null") ", "
               (string-literal (name s)) ")")))

(defn- emit-const [ctx node v]
  (cond
    (nil? v) (pure "null")
    (boolean? v) (pure (str v))
    (number? v) (pure (number-js v))
    (string? v) (pure (string-literal v))
    (char? v) (pure (string-literal (str v)))
    (keyword? v) (pure (keyword-js ctx v))
    (symbol? v) (pure (symbol-js ctx v))
    (instance? Pattern v) (pure (const! ctx [:regex (str v)]
                                        (str "new RegExp(" (string-literal (.pattern ^Pattern v)) ")")))
    ;; a Date is mutable, so each evaluation makes one
    (instance? Instant v) (no-effect (result [] (str "new Date(" (string-literal (str v)) ")")))
    (coll? v) (unsupported node "a constant collection, which needs core's collections (M2),")
    :else (unsupported node (str "a constant of class " (.getName (class v))))))

;; --- vars ------------------------------------------------------------------

(defn- var-place
  "The property holding var `q`: its namespace object and its name."
  [ctx q]
  [(alias! ctx (symbol (namespace q))) (names/munge (name q))])

(defn- var-js [ctx q]
  (let [[a p] (var-place ctx q)]
    (str a (names/property p string-literal))))

;; --- the special forms -----------------------------------------------------

(defn- boolean-type? [{:keys [base nil?]}]
  (and (= 'boolean base) (not nil?)))

(defn- emit-if [ctx {:keys [test then else]}]
  (let [tr (emit ctx test)]
    (or (short-circuit [tr])
        (let [c (if (boolean-type? (:type test)) (:expr tr) (str "$rt.truth(" (:expr tr) ")"))
              th (emit ctx then)
              el (emit ctx else)]
          (cond
            (and (:terminal? th) (:terminal? el))
            (terminal (result (-> (:stmts tr)
                                  (conj (str "if (" c ") {")) (into (indent (:stmts th)))
                                  (conj "} else {") (into (indent (:stmts el))) (conj "}"))
                              nil))

            (and (empty? (:stmts th)) (empty? (:stmts el)) (not (:terminal? th)) (not (:terminal? el)))
            (result (:stmts tr) (str "(" c " ? " (:expr th) " : " (:expr el) ")"))

            :else
            (let [v (temp! ctx)]
              (result (-> (:stmts tr)
                          (conj (str "let " v ";") (str "if (" c ") {"))
                          (into (indent (assign-lines th v)))
                          (conj "} else {")
                          (into (indent (assign-lines el v)))
                          (conj "}"))
                      v)))))))

(defn- emit-do [ctx {:keys [statements ret]}]
  (let [rs (mapv #(emit ctx %) statements)
        kept (vec (take-while (complement :terminal?) rs))
        cut (first (drop (count kept) rs))
        head (into [] (mapcat effect-stmts) kept)]
    (if cut
      (terminal (result (into head (:stmts cut)) nil))
      (let [r (emit ctx ret)]
        (assoc r :stmts (into head (:stmts r)))))))

(defn- emit-bindings
  "Each binding's init, in order, into a local of its own, or into
  `carriers` when given, the binding's own name declared after it for the
  inits that follow. {:stmts :terminal?}."
  [ctx bindings carriers]
  (loop [bs (seq bindings) cs (seq carriers) stmts []]
    (if-let [b (first bs)]
      (let [r (emit ctx (:init b))
            stmts (into stmts (:stmts r))]
        (if (:terminal? r)
          {:stmts stmts :terminal? true}
          (let [own (local! ctx b)]
            (recur (next bs) (next cs)
                   (if cs
                     (conj stmts (str "let " (first cs) " = " (:expr r) ";")
                           (str "let " own " = " (first cs) ";"))
                     (cond-> (conj stmts (str "const " own " = " (:expr r) ";"))
                       (report ctx b) (conj (report ctx b))))))))
      {:stmts stmts})))

(defn- emit-let [ctx {:keys [bindings body]}]
  (let [{:keys [stmts terminal?]} (emit-bindings ctx bindings nil)]
    (if terminal?
      (terminal (result stmts nil))
      (let [r (emit ctx body)]
        (assoc r :stmts (into stmts (:stmts r)))))))

(defn- fresh-line
  "The line that opens each iteration: the bindings' own names, declared
  from the carriers."
  [ctx bindings carriers]
  (when (seq bindings)
    [(str "const " (comma (map (fn [b c] (str (local! ctx b) " = " c)) bindings carriers)) ";")]))

(defn- emit-loop [ctx {:keys [id bindings body recurs?] :as node}]
  (if-not recurs?
    (emit-let ctx node)
    (let [cs (mapv (fn [_] (temp! ctx)) bindings)
          {:keys [stmts terminal?]} (emit-bindings ctx bindings cs)]
      (if terminal?
        (terminal (result stmts nil))
        (let [v (temp! ctx)
              ;; a local site reports at the top of each iteration
              fresh (into (vec (fresh-line ctx bindings cs)) (keep #(report ctx %)) bindings)
              r (emit (assoc-in ctx [:recur id] cs) body)]
          (result (-> stmts
                      (conj (str "let " v ";") "while (true) {")
                      (into (indent (cond-> (into (vec fresh) (:stmts r))
                                      (not (:terminal? r)) (conj (str v " = " (:expr r) ";") "break;"))))
                      (conj "}"))
                  v))))))

(defn- emit-recur [ctx {:keys [target exprs]}]
  (let [rs (mapv #(emit ctx %) exprs)]
    (or (short-circuit rs)
        (let [carriers (get-in ctx [:recur target])
              {:keys [stmts exprs]} (in-order ctx rs (> (count rs) 1))]
          (terminal (result (-> stmts
                                (into (map (fn [c e] (str c " = " e ";")) carriers exprs))
                                (conj "continue;"))
                            nil))))))

(defn- emit-throw [ctx {:keys [exception]}]
  (let [r (emit ctx exception)]
    (or (short-circuit [r])
        (terminal (result (conj (:stmts r) (str "throw " (:expr r) ";")) nil)))))

(defn- pure-def [r] (-> r stable no-effect))

(defn- emit-def [ctx {q :var init :init}]
  (let [[a p] (var-place ctx q)]
    (if init
      (let [r (if (= :fn (:op init))
                (emit-fn ctx init (str q))
                (emit ctx init))]
        (or (short-circuit [r])
            (pure-def (result (conj (:stmts r) (str "$rt.define(" a ", " (string-literal p) ", " (:expr r) ");"))
                              "null"))))
      (pure-def (result [(str "$rt.declare(" a ", " (string-literal p) ", " (string-literal (name q)) ");")]
                        "null")))))

(defn- emit-set! [ctx {:keys [target val] :as node}]
  (case (:op target)
    :var (let [[a p] (var-place ctx (:var target))]
           (emit-all ctx [val] (fn [[v]] (str "$rt.setVar(" a ", " (string-literal p) ", " v ")"))))
    :host-field (emit-all ctx [(:target target) val]
                          (fn [[o v]] (str "((" o ")." (names/host-name (:field target)) " = " v ")")))
    (unsupported node "set! of a local field")))

;; --- case ------------------------------------------------------------------

(defn- case-group
  "How a case test is switched on in JS (doc/ir.md §4.4): [group label]."
  [node v]
  (cond
    (keyword? v) [:keyword (string-literal (subs (str v) 1))]
    (symbol? v) [:symbol (string-literal (str v))]
    (nil? v) [:value "null"]
    (boolean? v) [:value (str v)]
    (number? v) [:value (number-js v)]
    (string? v) [:value (string-literal v)]
    (char? v) [:value (string-literal (str v))]
    :else (unsupported node (str "a case test of class " (.getName (class v))))))

(def ^:private group-order [:value :keyword :symbol])

(defn- discriminant
  "What a group's switch switches on. A value of another kind is null for
  the keyword and symbol groups, which no label of theirs is."
  [group v]
  (case group
    :value v
    :keyword (str "(" v " instanceof $rt.Keyword ? " v ".fqn : null)")
    :symbol (str "(" v " instanceof $rt.Symbol ? " v ".toString() : null)")))

(defn- emit-case [ctx {:keys [test nodes default] :as node}]
  (let [tr (emit ctx test)]
    (or (short-circuit [tr])
        (let [v (if (:stable? tr) (:expr tr) (temp! ctx))
              head (cond-> (vec (:stmts tr))
                     (not (:stable? tr)) (conj (str "const " v " = " (:expr tr) ";")))
              res (temp! ctx)
              labelled (for [[i n] (map-indexed vector nodes)
                             t (:tests n)]
                         (conj (case-group t (:val t)) i))
              groups (group-by first labelled)
              arms (mapv #(emit ctx (:then %)) nodes)
              dflt (if default
                     (assign-lines (emit ctx default) res)
                     [(str "throw $rt.noMatch(" v ");")])
              arm-lines (fn [i] (conj (assign-lines (nth arms i) res) "break;"))
              switch-lines
              (fn [disc cases tail]
                (-> [(str "switch (" disc ") {")]
                    (into (indent (mapcat (fn [[labels body]]
                                            (-> (mapv #(str "case " % ":") (butlast labels))
                                                (conj (str "case " (last labels) ": {"))
                                                (into (indent body))
                                                (conj "}")))
                                          cases)))
                    (into (indent tail))
                    (conj "}")))
              by-arm (fn [entries] (->> entries
                                        (group-by #(nth % 2))
                                        (sort-by key)
                                        (map (fn [[i es]] [i (mapv second es)]))))
              lines
              (if (<= (count groups) 1)
                (let [[g entries] (first groups)
                      g (or g :value)
                      disc (discriminant g v)]
                  (switch-lines disc (map (fn [[i ls]] [ls (arm-lines i)]) (by-arm entries))
                                (-> ["default: {"] (into (indent dflt)) (conj "}"))))
                ;; tests of several kinds: find the arm, then run it
                (let [arm (temp! ctx)]
                  (-> [(str "let " arm " = -1;")]
                      (into (mapcat (fn [g]
                                      (when-let [entries (get groups g)]
                                        (switch-lines (discriminant g v)
                                                      (map (fn [[i ls]] [ls [(str arm " = " i ";") "break;"]])
                                                           (by-arm entries))
                                                      [])))
                                    group-order))
                      (into (switch-lines arm (map (fn [i] [[i] (arm-lines i)]) (range (count nodes)))
                                          (-> ["default: {"] (into (indent dflt)) (conj "}")))))))]
          (result (-> head (conj (str "let " res ";")) (into lines)) res)))))

;; --- try -------------------------------------------------------------------

(defn- emit-try
  "try, with a temporary for the value. The catch clauses are one JS catch,
  testing the class of each in turn; nothing matching rethrows."
  [ctx {:keys [body catches finally]}]
  (let [b (emit ctx body)]
    (if (and (empty? catches) (nil? finally))
      b
      (let [v (temp! ctx)
            e (temp! ctx)
            clause (fn [{:keys [local] k :class :as c}]
                     (let [own (local! ctx local)
                           r (emit ctx (:body c))]
                       {:test (when k (emit ctx k))
                        :lines (into [(str "const " own " = " e ";")] (assign-lines r v))}))
            cs (mapv clause catches)
            catch-lines
            (when (seq cs)
              (let [chain (reduce (fn [else {:keys [test lines]}]
                                    (if test
                                      (-> (vec (:stmts test))
                                          (conj (str "if (" e " instanceof " (:expr test) ") {"))
                                          (into (indent lines))
                                          (conj "} else {")
                                          (into (indent else))
                                          (conj "}"))
                                      lines))
                                  [(str "throw " e ";")]
                                  (rseq cs))]
                (-> [(str "} catch (" e ") {")] (into (indent chain)))))
            f (when finally (emit ctx finally))]
        (result (-> [(str "let " v ";") "try {"]
                    (into (indent (assign-lines b v)))
                    (into catch-lines)
                    (cond-> f (-> (conj "} finally {") (into (indent (effect-stmts f)))))
                    (conj "}"))
                v)))))

(defn- emit-locking
  "JS has one thread per realm: the lock is evaluated, then the body
  (doc/ir.md §4.2)."
  [ctx {:keys [lock body]}]
  (let [l (emit ctx lock)]
    (or (short-circuit [l])
        (let [r (emit ctx body)]
          (assoc r :stmts (-> (effect-stmts l) (into (:stmts r))))))))

;; --- functions -------------------------------------------------------------

(defn- entered
  "The lines of a method's body, `lines` called with the ctx to emit it in.
  Under recording, a method that is an enter site tells the hook it was
  entered, with `args`, the JS names holding its parameters' values, and
  that its call is over in a finally, around everything, the loop a recur
  jumps to included. Its outer call is the call it was made in, lexically:
  a closure keeps it. A method that isn't a site reports in that call."
  [ctx m args lines]
  (if-let [site (and (:hook? ctx) (:site m))]
    (let [c (temp! ctx)]
      (-> [(str "const " c " = $hook.enter(" site ", " (:call ctx) ", [" (comma args) "]);")
           "try {"]
          (into (indent (lines (assoc ctx :call c))))
          (conj "} finally {" (str "  $hook.exit(" c ");") "}")))
    (lines ctx)))

(defn- method-lines
  "The body of one method. `params` are the JS names its parameters arrive
  in; the binding's own names are declared from them, inside the loop when
  the method is recurred to."
  [ctx {:keys [id body] ps :params :as m} recurs? incoming]
  (if recurs?
    (let [carriers (mapv (fn [_] (temp! ctx)) ps)]
      (-> (mapv (fn [c in] (str "let " c " = " in ";")) carriers incoming)
          (into (entered ctx m carriers
                         (fn [ctx]
                           (-> ["while (true) {"]
                               (into (indent (into (vec (fresh-line ctx ps carriers))
                                                   (body-lines (assoc-in ctx [:recur id] carriers) body))))
                               (conj "}")))))))
    (let [own (mapv #(local! ctx %) ps)]
      (into (mapv (fn [o in] (str "const " o " = " in ";")) own incoming)
            (entered ctx m own #(body-lines % body))))))

(defn- recur-targets [node]
  (into #{} (keep #(when (= :recur (:op %)) (:target %))) (ir/nodes node)))

(defn- arity-check [cond fn-name]
  (str "if (" cond ") $rt.arity(arguments.length, " (string-literal fn-name) ");"))

(defn- emit-fn
  "A function. One arity takes its parameters as formals; several dispatch
  on arguments.length. A variadic arity's rest parameter is an array, or
  null when it is empty."
  [ctx {:keys [local methods async?] :as node} fn-name]
  (let [self (when local (local! ctx local))
        fn-name (or fn-name (str (:ns ctx) "/" (or (some-> local :name) "fn")))
        targets (recur-targets node)
        recurs? #(contains? targets (:id %))
        head (str "(" (when async? "async ") "function" (when self (str " " self)))
        rest-arg (fn [k] (str "$rt.restArgs(Array.prototype.slice.call(arguments, " k "))"))
        incoming (fn [{:keys [variadic? fixed-arity params]}]
                   (cond-> (mapv #(str "arguments[" % "]") (range fixed-arity))
                     variadic? (conj (rest-arg fixed-arity))))]
    (pure
     (if (= 1 (count methods))
       (let [{:keys [variadic? fixed-arity] :as m} (first methods)
             plain? (not (or variadic? (recurs? m)))
             formals (if plain?
                       (mapv #(local! ctx %) (:params m))
                       (mapv (fn [_] (temp! ctx)) (range fixed-arity)))
             in (cond-> formals variadic? (conj (rest-arg fixed-arity)))]
         (str/join "\n"
                   (concat [(str head "(" (comma formals) ") {")]
                           (indent (into [(arity-check (if variadic?
                                                         (str "arguments.length < " fixed-arity)
                                                         (str "arguments.length !== " fixed-arity))
                                                       fn-name)]
                                         (if plain?
                                           (entered ctx m formals #(body-lines % (:body m)))
                                           (method-lines ctx m (recurs? m) in))))
                           ["})"])))
       (let [fixed (remove :variadic? methods)
             var-m (first (filter :variadic? methods))]
         (str/join "\n"
                   (concat [(str head "() {")]
                           (indent (-> ["switch (arguments.length) {"]
                                       (into (indent (mapcat (fn [m]
                                                               (-> [(str "case " (:fixed-arity m) ": {")]
                                                                   (into (indent (method-lines ctx m (recurs? m) (incoming m))))
                                                                   (conj "}")))
                                                             fixed)))
                                       (conj "}")
                                       (into (when var-m
                                               (-> [(str "if (arguments.length >= " (:fixed-arity var-m) ") {")]
                                                   (into (indent (method-lines ctx var-m (recurs? var-m) (incoming var-m))))
                                                   (conj "}"))))
                                       (conj (str "return $rt.arity(arguments.length, " (string-literal fn-name) ");"))))
                           ["})"])))))))

;; --- calls and interop -----------------------------------------------------

(defn- emit-invoke [ctx {f :fn args :args}]
  (if (= :var (:op f))
    (let [callee (var-js ctx (:var f))]
      (emit-all ctx args (fn [as] (str callee "(" (comma as) ")"))))
    (emit-all ctx (cons f args) (fn [[fe & as]] (str "(" fe ")(" (comma as) ")")))))

(defn- emit-host-call [ctx {:keys [target method args]}]
  (emit-all ctx (cons target args)
            (fn [[o & as]] (str "(" o ")." (names/host-name method) "(" (comma as) ")"))))

(defn- emit-host-field [ctx {:keys [target field]}]
  (emit-all ctx [target] (fn [[o]] (str "(" o ")." (names/host-name field)))))

(defn- emit-new [ctx {k :class args :args}]
  (emit-all ctx (cons k args) (fn [[c & as]] (str "new (" c ")(" (comma as) ")"))))

(defn- global-js [n]
  (str/join "." (map names/host-name (str/split n #"\."))))

(defn- emit-inline [ctx {:keys [template args]}]
  (emit-all ctx args
            (fn [as] (str "(" (apply str (interleave template (concat (map #(str "(" % ")") as) [""]))) ")"))))

;; --- dispatch --------------------------------------------------------------

(defn- emit-node [ctx node]
  (case (:op node)
    :const (emit-const ctx node (:val node))
    :quote (emit-const ctx node (:val (:expr node)))
    :if (emit-if ctx node)
    :do (emit-do ctx node)
    :let (emit-let ctx node)
    :loop (emit-loop ctx node)
    :recur (emit-recur ctx node)
    :local (pure (local-ref ctx (:id node)))
    :var (result [] (var-js ctx (:var node)))
    :the-var (let [[a p] (var-place ctx (:var node))]
               (no-effect (result [] (str "new $rt.Var(" a ", " (string-literal p) ", "
                                          (string-literal (name (:var node))) ")"))))
    :def (emit-def ctx node)
    :set! (emit-set! ctx node)
    :fn (emit-fn ctx node nil)
    :invoke (emit-invoke ctx node)
    :throw (emit-throw ctx node)
    :try (emit-try ctx node)
    :case (emit-case ctx node)
    :locking (emit-locking ctx node)
    :host-call (emit-host-call ctx node)
    :host-field (emit-host-field ctx node)
    :new (emit-new ctx node)
    :await (emit-all ctx [(:expr node)] (fn [[e]] (str "(await " e ")")))
    :js/global (result [] (global-js (:name node)))
    :js/inline (emit-inline ctx node)
    :js/array (emit-all ctx (:items node) (fn [es] (str "[" (comma es) "]")))
    :js/object (emit-all ctx (:vals node)
                         (fn [es] (str "({" (comma (map #(str (string-literal %1) ": " %2) (:keys node) es)) "})")))
    :ns (do (alias! ctx (:name node))
            (pure "null"))
    (:vector :map :set) (unsupported node "a collection literal, which needs core's collections (M2),")
    :with-meta (unsupported node "metadata on a literal")
    :letfn (unsupported node "letfn")
    :qualified-method (unsupported node "a qualified method used as a value")
    (unsupported node (str "the op " (:op node)))))

(defn emit
  "Emit `node`: {:stmts :expr :terminal? :stable? :no-effect?}. Under
  recording, a value site hands its value to the hook where the expression
  is evaluated, so a branch that recurs or throws, which never has one,
  reports nothing."
  [ctx node]
  (let [r (emit-node ctx node)
        site (and (:hook? ctx) (:site node))]
    (if (and site (not (:terminal? r)) (:expr r))
      (result (:stmts r) (str "$hook.value(" site ", (" (:expr r) "), " (:call ctx) ")"))
      r)))

;; --- forms -----------------------------------------------------------------

(defn form-js
  "The JS of a top-level form, `node`, compiled in namespace `ns`: a
  function expression that takes the runtime and returns the form's value.
  `hook?`: the form is compiled to be recorded, its sites marked, and the
  function takes the hook after the runtime."
  ([ns node] (form-js ns node false))
  ([ns node hook?]
   (let [ctx {:ns ns :n (volatile! 0) :names (volatile! {}) :consts (volatile! [])
              :const-ids (volatile! {}) :aliases (volatile! {}) :recur {}
              :hook? hook? :call "null"}
         body (body-lines ctx node)]
     (str/join "\n"
               (concat [(if hook? "(function ($rt, $hook) {" "(function ($rt) {") "\"use strict\";"]
                       (for [[n a] (sort-by val @(:aliases ctx))]
                         (str "const " a " = $rt.namespace(" (string-literal (str n)) ");"))
                       (for [[c init] @(:consts ctx)]
                         (str "const " c " = " init ";"))
                       body
                       ["})"])))))
