(ns replique.reader.forms
  "CST to forms (doc/reader.md §3): the data macros and the analyzer receive.

  This is where the reader depends on its context: the target (numbers, and the
  features reader conditionals select on), the current namespace and its
  aliases (::keywords, syntax-quote), and the namespace's tagged-literal
  readers. replique.reader.cst depends on none of it.

  Every form that can carry metadata carries its span: :line :column
  :end-line :end-column, and :file when known, the keys Clojure's reader uses.
  Numbers, strings, keywords, characters, nil and booleans can't carry
  metadata, so a collection records the spans of those elements itself, under
  :replique.reader/spans (see element-span)."
  (:require [clojure.instant :as instant]
            [replique.reader :as-alias reader]
            [clojure.string :as str]
            [replique.reader.cst :as cst])
  (:import [clojure.lang BigInt IObj Numbers Ratio]
           [java.time Instant OffsetDateTime ZoneOffset]
           [java.util UUID]
           [java.util.regex Pattern PatternSyntaxException]))

(def special-forms
  "The special forms of doc/ir.md. Syntax-quote leaves them unqualified."
  '#{def if do let* letfn* loop* recur fn* quote var throw try catch finally
     new . set! case* deftype* reify* await locking ns & js* definterface})

(def reader-keys
  "The metadata keys the reader adds, which syntax-quote and quote leave out."
  #{:line :column :end-line :end-column :file ::reader/spans})

;; --- errors ----------------------------------------------------------------

(defn- fail
  "Abandon the current top-level form with a diagnostic on `node`."
  [node code message & {:as more}]
  (throw (ex-info message {::diagnostic (merge {:severity :error :code code :message message
                                                :start (:start node) :end (:end node)}
                                               more)})))

(defn- warn! [ctx node code message]
  (vswap! (:diagnostics ctx) conj {:severity :warning :code code :message message
                                   :start (:start node) :end (:end node)}))

(defn- next-id! [ctx]
  (swap! (:gensym-counter ctx) inc))

;; --- spans -----------------------------------------------------------------

(defn- node-span [ctx node]
  (cst/span (:parse ctx) node))

(defn- with-span
  "`form` with the span of `node`, under any metadata it already has."
  [ctx node form]
  (if (instance? IObj form)
    (let [m (meta form)
          span (node-span ctx node)]
      (with-meta form (if m (merge span m) span)))
    form))

(defn- carries-meta? [x] (instance? IObj x))

;; --- atoms -----------------------------------------------------------------

(defn- integer-value [^String text]
  (let [m (re-matches cst/int-pattern text)
        [_ sign zero dec hex oct radix radix-digits big] m
        negate (fn [^BigInteger n] (if (= "-" sign) (.negate n) n))]
    (if zero
      (if big (BigInt/fromBigInteger BigInteger/ZERO) 0)
      (let [[digits base] (cond dec [dec 10] hex [hex 16] oct [oct 8]
                                radix [radix-digits (Integer/parseInt radix)])
            n (negate (BigInteger. ^String digits (int base)))]
        (cond
          big (BigInt/fromBigInteger n)
          (< (.bitLength n) 64) (.longValue n)
          :else (BigInt/fromBigInteger n))))))

(defn- jvm-number
  "The number `text` denotes, with Clojure's rules."
  [node ^String text]
  (cond
    (re-matches cst/int-pattern text) (integer-value text)

    (re-matches cst/ratio-pattern text)
    (let [[_ n d] (re-matches cst/ratio-pattern text)]
      (try (Numbers/divide (BigInteger. ^String n) (BigInteger. ^String d))
           (catch ArithmeticException _
             (fail node :reader/invalid-number (str "division by zero in " text)))))

    :else
    (let [[_ digits _ _ big] (re-matches cst/float-pattern text)]
      (if big (BigDecimal. ^String digits) (Double/parseDouble text)))))

(def ^:private max-safe-integer 9007199254740991)

(defn- number-value
  "The number of a :number leaf, for the target. In JS every number is a
  double (doc/language.md §5): a ratio reads as its quotient, an integer beyond
  ±2^53 reads rounded with a warning, 1N is a BigInt, and 1M doesn't exist."
  [ctx node]
  (let [text (:text node)
        v (jvm-number node text)]
    (if (not= :js (:target ctx))
      v
      (cond
        (decimal? v) (fail node :reader/bigdecimal-in-js
                           (str text " is a BigDecimal, which JS doesn't have"))
        (and (instance? BigInt v) (str/ends-with? text "N")) v
        (instance? Ratio v) (double v)
        (and (integer? v) (> (abs (bigint v)) max-safe-integer))
        (do (warn! ctx node :reader/imprecise-integer
                   (str text " is beyond ±2^53 and can't be exact in JS; write "
                        text "N for a BigInt"))
            (double v))
        :else v))))

(def ^:private char-names
  {"newline" \newline "space" \space "tab" \tab "backspace" \backspace
   "formfeed" \formfeed "return" \return})

(defn- char-value [node]
  (let [s (subs (:text node) 1)]
    (cond
      (= 1 (count s)) (.charAt s 0)
      (char-names s) (char-names s)
      (re-matches #"u[0-9A-Fa-f]{4}" s) (char (Integer/parseInt (subs s 1) 16))
      (re-matches #"o[0-7]{1,3}" s)
      (let [n (Integer/parseInt (subs s 1) 8)]
        (if (<= n 0377) (char n) (fail node :reader/invalid-char (str "octal escape out of range: \\" s))))
      :else (fail node :reader/invalid-char (str "unknown character \\" s)))))

(defn- string-value [node]
  (let [^String t (:text node)
        end (dec (count t))
        sb (StringBuilder.)]
    (loop [i 1]
      (if (>= i end)
        (str sb)
        (let [c (.charAt t i)]
          (if (not= \\ c)
            (do (.append sb c) (recur (inc i)))
            (let [e (.charAt t (inc i))]
              (case e
                \t (do (.append sb \tab) (recur (+ i 2)))
                \r (do (.append sb \return) (recur (+ i 2)))
                \n (do (.append sb \newline) (recur (+ i 2)))
                \b (do (.append sb \backspace) (recur (+ i 2)))
                \f (do (.append sb \formfeed) (recur (+ i 2)))
                \\ (do (.append sb \\) (recur (+ i 2)))
                \" (do (.append sb \") (recur (+ i 2)))
                \u (let [hex (subs t (+ i 2) (min end (+ i 6)))]
                     (if (re-matches #"[0-9A-Fa-f]{4}" hex)
                       (do (.append sb (char (Integer/parseInt hex 16))) (recur (+ i 6)))
                       (fail node :reader/invalid-escape (str "invalid unicode escape \\u" hex))))
                (if (<= 0 (Character/digit e 8))
                  (let [oct (re-find #"^[0-7]{1,3}" (subs t (inc i) end))
                        n (Integer/parseInt oct 8)]
                    (when (> n 0377)
                      (fail node :reader/invalid-escape (str "octal escape out of range: \\" oct)))
                    (.append sb (char n))
                    (recur (+ i 1 (count oct))))
                  (fail node :reader/invalid-escape (str "unsupported escape \\" e)))))))))))

(defn- regex-value [node]
  (let [t (:text node)
        source (subs t 2 (dec (count t)))]
    (try (Pattern/compile source)
         (catch PatternSyntaxException e
           (fail node :reader/invalid-regex (.getDescription e))))))

(defn- symbolic-value [node]
  (case (:text node)
    "##Inf" Double/POSITIVE_INFINITY
    "##-Inf" Double/NEGATIVE_INFINITY
    "##NaN" Double/NaN
    (fail node :reader/invalid-token (str "unknown symbolic value " (:text node)))))

(defn- alias-ns [ctx node alias]
  (or (get (:aliases ctx) (symbol alias))
      (fail node :reader/unknown-alias (str "no namespace aliased " alias " in " (:ns ctx)))))

(defn- current-ns [ctx node]
  (or (:ns ctx)
      (fail node :reader/no-namespace "::keywords need a current namespace")))

(defn- keyword-value [ctx node]
  (let [t (:text node)]
    (if (str/starts-with? t "::")
      (let [s (symbol (subs t 2))]
        (if-let [a (namespace s)]
          (keyword (str (alias-ns ctx node a)) (name s))
          (keyword (str (current-ns ctx node)) (name s))))
      (keyword (subs t 1)))))

(defn- fn-arg
  "Inside #(...), the parameter a % symbol stands for, or nil."
  [ctx ^String text]
  (when-let [args (:fn-args ctx)]
    (when-let [[_ n] (re-matches #"%(&|[1-9][0-9]*)?" text)]
      (let [k (cond (nil? n) 1 (= "&" n) :rest :else (Long/parseLong n))]
        (or (get @args k)
            (let [sym (symbol (if (= :rest k)
                                (str "rest__" (next-id! ctx) "#")
                                (str "p" k "__" (next-id! ctx) "#")))]
              (swap! args assoc k sym)
              sym))))))

(defn- symbol-value [ctx node]
  (let [t (:text node)]
    (case t
      "nil" nil
      "true" true
      "false" false
      (or (fn-arg ctx t) (symbol t)))))

;; --- collections -----------------------------------------------------------

(declare convert convert-one)

(defn- convert-children
  "The forms of a container's children, each with the node it came from:
  [[form node] ...]. A splicing reader conditional gives several forms for
  one node. Spans are computed later, and only for the elements that need
  them."
  [ctx node]
  (persistent!
   (reduce (fn [acc child]
             (case (:kind child)
               (:whitespace :comment :discard) acc
               (reduce #(conj! %1 [%2 child]) acc (convert ctx child))))
           (transient [])
           (:children node))))

(defn- element-spans-by-index
  "Spans for the elements that can't carry them, by index, or nil if none."
  [ctx pairs]
  (let [n (count pairs)]
    (loop [i 0 spans nil]
      (if (< i n)
        (let [[f child] (nth pairs i)]
          (recur (inc i)
                 (if (carries-meta? f)
                   spans
                   (assoc (or spans (vec (repeat n nil))) i (node-span ctx child)))))
        spans))))

(defn- with-element-spans [form spans]
  (if spans (vary-meta form assoc ::reader/spans spans) form))

(defn- same-key
  "The value two keys are compared by. In JS, 1 and 1.0 are one number."
  [ctx k]
  (if (and (= :js (:target ctx)) (number? k) (not (instance? BigInt k)))
    (double k)
    k))

(defn- check-distinct!
  "Fail if two of `ks` are the same key. `built` is the collection made from
  them: if it lost an element, two were equal; in JS, numbers are compared as
  doubles too."
  [ctx node ks built what]
  (when (or (< (count built) (count ks))
            (and (= :js (:target ctx)) (some number? ks)))
    (when-let [dup (some (fn [[_ fs]] (when (> (count fs) 1) (first fs)))
                         (group-by #(same-key ctx %) ks))]
      (fail node :reader/duplicate-key
            (str "duplicate " what " " (pr-str dup) " in a literal")))))

(defn- build-map
  "A map from converted [form node] pairs, keys transformed by `key-fn`."
  [ctx node pairs key-fn]
  (when (odd? (count pairs))
    (fail node :reader/map-odd "a map literal needs an even number of forms"))
  (let [n (count pairs)
        kvs (object-array n)
        _ (dotimes [i n]
            (let [f (first (nth pairs i))]
              (aset kvs i (if (even? i) (key-fn f) f))))
        ks (vec (take-nth 2 kvs))
        m (if (<= n 16)
            (clojure.lang.PersistentArrayMap/createAsIfByAssoc kvs)
            (clojure.lang.PersistentHashMap/create kvs))]
    (check-distinct! ctx node ks m "key")
    (let [spans (loop [i 0 spans (transient {})]
                  (if (< i n)
                    (let [k (aget kvs i) v (aget kvs (inc i))]
                      (recur (+ i 2)
                             (if (and (carries-meta? k) (carries-meta? v))
                               spans
                               (assoc! spans k [(node-span ctx (second (nth pairs i)))
                                                (node-span ctx (second (nth pairs (inc i))))]))))
                    (persistent! spans)))]
      (with-element-spans m (not-empty spans)))))

(defn- build-set [ctx node pairs]
  (let [ks (mapv first pairs)
        s (set ks)]
    (check-distinct! ctx node ks s "element")
    (with-element-spans s (not-empty (into {} (keep (fn [[f child]]
                                                      (when-not (carries-meta? f)
                                                        [f (node-span ctx child)])))
                                           pairs)))))

(defn- build-list [ctx pairs]
  (with-element-spans (clojure.lang.PersistentList/create (mapv first pairs))
    (element-spans-by-index ctx pairs)))

(defn- build-vector [ctx pairs]
  (with-element-spans (mapv first pairs) (element-spans-by-index ctx pairs)))

(defn- namespaced-key [ns-str k]
  (cond
    (and (keyword? k) (nil? (namespace k))) (keyword ns-str (name k))
    (and (keyword? k) (= "_" (namespace k))) (keyword (name k))
    (and (symbol? k) (nil? (namespace k))) (with-meta (symbol ns-str (name k)) (meta k))
    (and (symbol? k) (= "_" (namespace k))) (with-meta (symbol (name k)) (meta k))
    :else k))

(defn- namespaced-map [ctx node]
  (let [open (:open node)
        ns-str (cond
                 (= "#::" open) (str (current-ns ctx node))
                 (str/starts-with? open "#::") (str (alias-ns ctx node (subs open 3)))
                 :else (subs open 2))
        [m & more] (cst/forms node)]
    (when (or more (not= :map (:kind m)))
      (fail node :reader/invalid-namespaced-map (str open " must be followed by a map")))
    (build-map ctx m (convert-children ctx m) #(namespaced-key ns-str %))))

(defn- fn-literal [ctx node]
  (when (:fn-args ctx)
    (fail node :reader/nested-fn "#() can't contain another #()"))
  (let [args (atom {})
        body (build-list ctx (convert-children (assoc ctx :fn-args args) node))
        m @args
        n (apply max 0 (filter number? (keys m)))
        params (vec (concat (for [i (range 1 (inc n))]
                              (or (get m i) (symbol (str "p" i "__" (next-id! ctx) "#"))))
                            (when-let [r (:rest m)] ['& r])))]
    (list 'fn* params (with-span ctx node body))))

;; --- reader conditionals ---------------------------------------------------

(defn- reader-cond [ctx node]
  (let [items (cst/forms node)]
    (when (odd? (count items))
      (fail node :reader/invalid-reader-cond
            "a reader conditional needs pairs of a feature and a form"))
    (or (some (fn [[k form-node]]
                (when-not (and (= :keyword (:kind k)) (not (str/starts-with? (:text k) "::")))
                  (fail k :reader/invalid-reader-cond "a feature is a plain keyword"))
                (let [feature (keyword (subs (:text k) 1))]
                  (when (or (contains? (:features ctx) feature) (= :default feature))
                    (let [form (convert-one ctx form-node)]
                      (if (:splicing? node)
                        (if (sequential? form)
                          (vec form)
                          (fail form-node :reader/invalid-reader-cond
                                "#?@ needs a list or vector to splice"))
                        [form])))))
              (partition 2 items))
        [])))

;; --- syntax-quote ----------------------------------------------------------

(defn- core [ctx sym]
  (symbol (str (:core-ns ctx)) (name sym)))

(defn- form-of? [ctx form sym]
  (and (seq? form) (= (core ctx sym) (first form))))

(defn default-resolve-symbol
  "How syntax-quote qualifies a symbol when the caller gives no resolver: an
  alias is expanded, a symbol with a dot in it is a class name and stays as it
  is, and any other unqualified symbol gets the current namespace. The
  analyzer passes its own, which knows referred vars and imported classes."
  [{:keys [ns aliases]} sym]
  (cond
    (namespace sym) (if-let [full (get aliases (symbol (namespace sym)))]
                      (symbol (str full) (name sym))
                      sym)
    (pos? (.indexOf (name sym) ".")) sym
    ns (symbol (str ns) (name sym))
    :else sym))

(defn- sq-symbol [ctx node gensyms sym]
  (let [n (name sym)]
    (cond
      (and (nil? (namespace sym)) (str/ends-with? n "#"))
      (or (get @gensyms sym)
          (let [g (symbol (str (subs n 0 (dec (count n))) "__" (next-id! ctx) "__auto__"))]
            (swap! gensyms assoc sym g)
            g))

      (and (nil? (namespace sym)) (str/starts-with? n ".")) sym

      (and (nil? (namespace sym)) (str/ends-with? n ".") (> (count n) 1))
      (let [r ((:resolve-symbol ctx) ctx (symbol (subs n 0 (dec (count n)))))]
        (symbol (namespace r) (str (name r) ".")))

      :else ((:resolve-symbol ctx) ctx sym))))

(defn- user-meta [form]
  (not-empty (apply dissoc (meta form) reader-keys)))

(declare syntax-quote)

(defn- sq-items [ctx node gensyms items]
  (cons (core ctx 'concat)
        (for [x items]
          (cond
            (form-of? ctx x 'unquote) (list (core ctx 'list) (second x))
            (form-of? ctx x 'unquote-splicing) (second x)
            :else (list (core ctx 'list) (syntax-quote ctx node gensyms x))))))

(defn- syntax-quote
  "The form that builds `form` at run time, as Clojure's syntax-quote does,
  with calls into the core namespace."
  [ctx node gensyms form]
  (let [c #(core ctx %)
        built (cond
                (and (symbol? form) ((:special-forms ctx) form)) (list 'quote form)
                (symbol? form) (list 'quote (sq-symbol ctx node gensyms form))
                (form-of? ctx form 'unquote) (second form)
                (form-of? ctx form 'unquote-splicing)
                (fail node :reader/splice-outside-list "~@ can only appear inside a collection")
                (or (keyword? form) (string? form) (number? form) (char? form))
                form
                (map? form) (list (c 'apply) (c 'hash-map)
                                  (list (c 'seq) (sq-items ctx node gensyms (apply concat form))))
                (vector? form) (list (c 'apply) (c 'vector)
                                     (list (c 'seq) (sq-items ctx node gensyms form)))
                (set? form) (list (c 'apply) (c 'hash-set)
                                  (list (c 'seq) (sq-items ctx node gensyms form)))
                (seq? form) (if (empty? form)
                              (list (c 'list))
                              (list (c 'seq) (sq-items ctx node gensyms form)))
                :else (list 'quote form))]
    (if-let [m (and (or (coll? form) (symbol? form)) (user-meta form))]
      (list (c 'with-meta) built (syntax-quote ctx node gensyms m))
      built)))

;; --- metadata and tags -----------------------------------------------------

(defn- meta-value [node m]
  (cond
    (keyword? m) {m true}
    (or (symbol? m) (string? m)) {:tag m}
    (map? m) (apply dissoc m reader-keys)
    (vector? m) {:param-tags m}
    :else (fail node :reader/invalid-meta
                "metadata is a keyword, a symbol, a string, a map or a vector")))

(defn- with-user-meta [ctx node]
  (let [[m-node target-node] (cst/forms node)
        m (meta-value m-node (convert-one ctx m-node))
        target (convert-one ctx target-node)]
    (when-not (carries-meta? target)
      (fail target-node :reader/invalid-meta
            (str "metadata can't be attached to " (pr-str target))))
    (vary-meta target merge m)))

(defn- inst-value [node s]
  (try
    (instant/parse-timestamp
     (fn [years months days hours minutes seconds nanoseconds offset-sign offset-hours offset-minutes]
       (.toInstant (OffsetDateTime/of years months days hours minutes seconds nanoseconds
                                      (ZoneOffset/ofHoursMinutes (* offset-sign offset-hours)
                                                                 (* offset-sign offset-minutes)))))
     s)
    (catch Exception e
      (fail node :reader/invalid-tagged (str "invalid #inst " (pr-str s) ": " (ex-message e))))))

(defn- tagged [ctx node]
  (let [tag (:tag node)
        [form-node] (cst/forms node)
        form (convert-one ctx form-node)]
    (case tag
      inst (if (string? form)
             (inst-value node form)
             (fail node :reader/invalid-tagged "#inst takes a string"))
      uuid (if (string? form)
             (try (UUID/fromString form)
                  (catch IllegalArgumentException _
                    (fail node :reader/invalid-tagged (str "invalid #uuid " (pr-str form)))))
             (fail node :reader/invalid-tagged "#uuid takes a string"))
      js (tagged-literal 'js form)
      (if-let [f (get (:tags ctx) tag)]
        (try (f form)
             (catch Exception e
               (fail node :reader/invalid-tagged
                     (str "the reader for #" tag " failed: " (ex-message e)))))
        (fail node :reader/unknown-tag
              (str "no reader for #" tag " in " (or (:ns ctx) "this namespace")))))))

;; --- dispatch --------------------------------------------------------------

(defn convert
  "The forms `node` stands for: usually one, none for a reader conditional
  with no matching branch, several for a splicing one."
  [ctx node]
  (case (:kind node)
    (:whitespace :comment :discard) []
    :reader-cond (reader-cond ctx node)
    [(let [k (:kind node)
           f (case k
               :symbol (symbol-value ctx node)
               :keyword (keyword-value ctx node)
               :number (number-value ctx node)
               :string (string-value node)
               :char (char-value node)
               :regex (regex-value node)
               :symbolic (symbolic-value node)
               :list (build-list ctx (convert-children ctx node))
               :vector (build-vector ctx (convert-children ctx node))
               :map (build-map ctx node (convert-children ctx node) identity)
               :set (build-set ctx node (convert-children ctx node))
               :namespaced-map (namespaced-map ctx node)
               :fn (fn-literal ctx node)
               :quote (list 'quote (convert-one ctx (first (cst/forms node))))
               :syntax-quote (syntax-quote ctx node (atom {})
                                           (convert-one ctx (first (cst/forms node))))
               :unquote (list (core ctx 'unquote) (convert-one ctx (first (cst/forms node))))
               :unquote-splicing (list (core ctx 'unquote-splicing)
                                       (convert-one ctx (first (cst/forms node))))
               :deref (list (core ctx 'deref) (convert-one ctx (first (cst/forms node))))
               :var (list 'var (convert-one ctx (first (cst/forms node))))
               :meta (with-user-meta ctx node)
               :tagged (tagged ctx node)
               :error (fail node :reader/invalid-token "unreadable"))]
       (if (= :meta k) f (with-span ctx node f)))]))

(defn convert-one
  "The single form `node` stands for."
  [ctx node]
  (let [fs (convert ctx node)]
    (if (= 1 (count fs))
      (first fs)
      (fail node :reader/missing-form
            "expected one form here, and this reader conditional gives none or several"))))

(defn context
  "The conversion context for a parse and the reader options."
  [parse {:keys [target features ns aliases resolve-symbol tags core-ns gensym-counter]
          sf :special-forms}]
  (let [target (or target :jvm)]
    {:parse parse
     :target target
     :features (or features #{target})
     :ns ns
     :aliases (or aliases {})
     :resolve-symbol (or resolve-symbol default-resolve-symbol)
     :tags (or tags {})
     :special-forms (or sf special-forms)
     :core-ns (or core-ns 'replique.core)
     :gensym-counter (or gensym-counter (atom 0))
     :diagnostics (volatile! [])}))

(defn top-level
  "The forms of one top-level node: [forms diagnostics]. A node with a syntax
  error gives no forms: its error is already among the parse's diagnostics."
  [ctx node]
  (cond
    (or (:error? node) (#{:whitespace :comment :discard} (:kind node))) [[] []]

    (and (= :reader-cond (:kind node)) (:splicing? node))
    [[] [{:severity :error :code :reader/invalid-reader-cond
          :message "#?@ can't splice at the top level" :start (:start node) :end (:end node)}]]

    :else
    (let [ctx (assoc ctx :diagnostics (volatile! []))]
      (try
        [(convert ctx node) @(:diagnostics ctx)]
        (catch clojure.lang.ExceptionInfo e
          (if-let [d (::diagnostic (ex-data e))]
            [[] (conj @(:diagnostics ctx) d)]
            (throw e)))))))

(defn element-span
  "The span of the element of `coll` at `k` (an index, a key, or a set
  element) when the element can't carry its own: a number, a string, a
  keyword... For a map, the key's and the value's spans as [k-span v-span].
  Nil when the collection wasn't read from source."
  [coll k]
  (get (::reader/spans (meta coll)) k))
