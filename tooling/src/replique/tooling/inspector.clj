(ns replique.tooling.inspector
  "A value, browsed a piece at a time (doc/server.md §6): the half of the
  tree an editor shows that holds the value. A node is a place in the
  value, its children are asked for a page at a time, and each comes back
  as a line to show, a short printing of it, what it is, how many things it
  holds, with the id of its node and whether there is more in it than the
  line shows. Ported from replique's replique.inspector, whose protocol
  this speaks.

  THE VALUE STAYS WHERE IT IS. A JVM value is here; a JS one is in Node,
  and what is here is a mirror of it, sent a part at a time by recording.js:
  a JsLeaf, printed there, or a JsColl, whose parts not sent yet are asked
  for by where it is, a root and a path through it. The rest is the same
  code for both.

  A NODE IS A PATH, and its id stands for that path across every value the
  view is given. A refresh puts a new value at the root; a node is found
  again in it by walking its path down from there, so what was open stays
  open, and what is no longer there says so.

  What is browsed is what the kernel has, before core and its collections
  exist: arrays, an exception's message, data and cause, what a var holds,
  a JS object's properties; and an outline, what a view of what was
  recorded says of values, which is not part of them."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [replique.js.emit :as js-emit]
            [replique.tooling.json :as json]
            [replique.tooling.recording :as recording])
  (:import [replique.lang Keyword Printer Var]))

(def gone
  "What a node is when its path no longer leads anywhere."
  ::gone)

;; --- what is not data -------------------------------------------------------

(deftype Outline [entries above])

(defn outline
  "What is shown as an outline rather than as data: a heading a line, its
  label, and the value under it, browsed as any other. `entries` are [label
  value] pairs, a label a string, said once: one said again is told apart
  by its number, \"x (2)\"; or [label value :above], the label on a line of
  its own, above its value: code, which can be long."
  [entries]
  (let [[out _ above] (reduce (fn [[out seen above] [label v where]]
                                (let [label (first (remove seen (cons label (map #(str label " (" % ")")
                                                                                 (iterate inc 2)))))]
                                  [(conj out [label v]) (conj seen label)
                                   (cond-> above (= :above where) (conj label))]))
                              [[] #{} #{}]
                              entries)]
    (Outline. out above)))

(defn outline? [x] (instance? Outline x))

(defn entries
  "The [label value] pairs of the outline `o`."
  [o]
  (.-entries ^Outline o))

(defn- above? [o label] (contains? (.-above ^Outline o) label))

(defn- outline-get [o label]
  (if-let [[_ v] (some #(when (= label (first %)) %) (entries o))]
    v
    gone))

(defn note
  "A word said in place of a value, `:not-kept`, as Replique prints it."
  [word]
  (Keyword/intern ^String word))

;; --- values in Node -----------------------------------------------------------

;; A value of Node that holds no parts, printed there, at `place`.
(deftype JsLeaf [text whole kind type place])

;; A value of Node that holds parts: its `id` there, by identity, its
;; `shape`, :seq, :object or :ref, how many parts it holds, and the ones
;; sent, an atom of {index [key value]}; `place`, {:rt :root :path}, where it
;; is, by which the others are asked for.
(deftype JsColl [id shape kind type text count loaded place])

(declare js-value)

(defn- js-place [{:keys [path] :as place} key] (assoc place :path (conj path key)))

(defn- load!
  "Take in the parts of `c` the mirror `m` sends."
  [^JsColl c m]
  (when-let [es (:entries m)]
    (swap! (.-loaded c) into
           (map-indexed (fn [i [k x]] [(+ (:from m) i) [k (js-value (js-place (.-place c) k) x)]]) es))))

(defn js-value
  "The value the mirror `m` of recording.js stands for, at `place`, {:rt
  :root :path}: a JsLeaf or a JsColl, or nil for nil."
  [place m]
  (cond
    (nil? m) nil
    (= "nil" (:kind m)) nil
    (:shape m) (doto (JsColl. (:id m) (keyword (:shape m)) (:kind m) (:type m) (:text m) (:count m)
                              (atom {}) place)
                 (load! m))
    :else (JsLeaf. (:text m) (:whole m) (:kind m) (:type m) place)))

(defn- js-array [xs] (recording/js-array xs))

(defn- node-json
  "What a method of recording.js answered, JSON, as Node printed it."
  [answer]
  (some-> answer edn/read-string json/read-str))

(defn- js-key [k] (if (string? k) (js-emit/string-literal k) (str k)))

(defn js-root
  "A root of a value of Node, as recording.js takes it."
  [root]
  (js-array (map #(if (string? %) (js-emit/string-literal %) (str %)) root)))

(defn- js-path [path] (js-array (map js-key path)))

(defn- ensure!
  "Make sure the parts of `c` numbered `from` to `to`, excluded, are sent:
  asked for where one is not."
  [^JsColl c from to]
  (let [to (min to (.-count c))]
    (when (and (< from to) (some #(not (contains? @(.-loaded c) %)) (range from to)))
      (let [{:keys [rt root path]} (.-place c)]
        (load! c (some-> (recording/in-node rt "entries" (js-root root) (js-path path) from (- to from))
                         node-json))))))

(defn- js-parts
  "The parts of `c` numbered `from` to `to`, excluded, that there are:
  [key value]s."
  [^JsColl c from to]
  (ensure! c from to)
  (let [loaded @(.-loaded c)]
    (keep #(get loaded %) (range from (min to (.-count c))))))

(defn- js-part
  "What the part of `c` keyed `k` holds, or `gone`."
  [^JsColl c k]
  (if (and (= :seq (.-shape c)) (integer? k))
    (if (< -1 k (.-count c)) (or (some-> (first (js-parts c k (inc k))) second) gone) gone)
    (if-let [[v] (some (fn [[key v]] (when (= k key) [v])) (js-parts c 0 (.-count c)))]
      v
      gone)))

(defn- js-printed
  "The value `x` of Node printed whole, up to `limit` chars, there: [text
  whole?]."
  [x limit]
  (if (and (instance? JsLeaf x) (or (.-whole ^JsLeaf x) (<= limit (count (.-text ^JsLeaf x)))))
    [(subs (.-text ^JsLeaf x) 0 (min limit (count (.-text ^JsLeaf x)))) (.-whole ^JsLeaf x)]
    (let [{:keys [rt root path]} (if (instance? JsColl x) (.-place ^JsColl x) (.-place ^JsLeaf x))]
      (or (some-> (recording/in-node rt "printedWhole" (js-root root) (js-path path) limit) node-json)
          ["#<gone>" true]))))

;; What a JVM value that is not a collection holds, [key value]s, browsed as
;; a JS object's properties are: an exception's message, data and cause.
(deftype Props [entries])

;; --- what a value is ----------------------------------------------------------

(defn- array? [x] (and (some? x) (.isArray (class x))))

(defn- shape
  "How `x` holds what it holds: :outline, :seq, :ref, :object or nil."
  [x]
  (cond
    (nil? x) nil
    (outline? x) :outline
    (instance? JsColl x) (.-shape ^JsColl x)
    (instance? Props x) :object
    (array? x) :seq
    (instance? Var x) :ref
    :else nil))

(defn- kind
  "What `x` is, in a word an editor can show it by."
  [x]
  (cond
    (nil? x) "nil"
    (instance? JsLeaf x) (.-kind ^JsLeaf x)
    (instance? JsColl x) (.-kind ^JsColl x)
    (boolean? x) "boolean"
    (string? x) "string"
    (number? x) "number"
    (char? x) "char"
    (instance? Keyword x) "keyword"
    (instance? replique.lang.Symbol x) "symbol"
    (instance? Var x) "var"
    (instance? replique.lang.IFn x) "fn"
    (array? x) "array"
    (outline? x) "outline"
    :else "object"))

(defn- type-name
  "The name of `x`'s type, the short way, or nil."
  [x]
  (cond
    (or (nil? x) (outline? x)) nil
    (instance? JsLeaf x) (.-type ^JsLeaf x)
    (instance? JsColl x) (.-type ^JsColl x)
    (instance? replique.lang.IFn x) "fn"
    :else (let [n (.getName (class x))
                ;; a hidden class, a function's, is named n/0x...
                n (subs n 0 (or (str/index-of n "/") (count n)))]
            (if (array? x) (.getSimpleName (class x)) (subs n (inc (or (str/last-index-of n ".") -1)))))))

(defn- counted
  "How many things `x` holds, where that is known."
  [x]
  (cond
    (outline? x) (count (entries x))
    (instance? JsColl x) (.-count ^JsColl x)
    (instance? Props x) (count (.-entries ^Props x))
    (array? x) (count x)
    :else nil))

(defn- datafiable?
  "Whether `x` is browsed as something else: an exception, as its message,
  data and cause."
  [x]
  (instance? Throwable x))

(defn- browsed
  "What the children of `x` are taken from."
  [x]
  (if (instance? Throwable x)
    (let [^Throwable t x]
      (Props. (filterv (comp some? second)
                       [["message" (.getMessage t)]
                        ["data" (when (instance? replique.lang.ExceptionInfo t) (.-data ^replique.lang.ExceptionInfo t))]
                        ["cause" (.getCause t)]])))
    x))

;; --- printing, within a bound --------------------------------------------------

(def ^:private full
  "What a printing that went past its bound is stopped with."
  (ex-info "The printing went past its bound" {::full true}))

(defn- write-outline
  "Write `o` as one line, each label and what is under it, the first
  `length` of them where there is one. Apart by two spaces, and by a dot
  where what is under them is an outline itself."
  [o write! write-value! length]
  (let [es (entries o)
        apart (if (some (fn [[_ v]] (outline? v)) es) " · " "  ")]
    (loop [[[label v] & more :as es] (seq es) i 0]
      (when es
        (if (and length (>= i length))
          (write! "...")
          (do (write! label)
              (write! " ")
              (write-value! v)
              (when more (write! apart))
              (recur more (inc i))))))))

(defn- print-within
  "`x` printed with the first `length` of what each thing holds, as many as
  `level` deep, and stopped past `limit` chars: [text whole? hidden?],
  whole? being whether it was not stopped, nor a part left out that Node
  did not send, and hidden? whether something
  printed holds what its printing does not show: an object, an exception,
  a var. As Replique prints it, and an outline as `write-outline` does."
  [x limit length level]
  (if (and (string? x) (> (count x) limit))
    [(subs (Printer/print (subs x 0 limit)) 0 limit) false false]
    (let [sb (StringBuilder.)
          left-out (volatile! false)
          hidden (volatile! false)
          write! (fn [^String s]
                   (.append sb s)
                   (when (> (.length sb) (long limit)) (throw full)))
          ;; what `xs` holds, `gone` for a part Node did not send, which
          ;; prints as ..., and what is after it with it
          items! (fn [w open xs close depth]
                   (if (and level (>= depth level))
                     (write! "#")
                     (do (write! open)
                         (loop [xs (seq xs) i 0]
                           (when xs
                             (when (pos? i) (write! " "))
                             (cond
                               (and length (>= i length)) (write! "...")
                               (identical? gone (first xs)) (do (write! "...") (vreset! left-out true))
                               :else
                               (do (w (first xs) (inc depth))
                                   (recur (next xs) (inc i))))))
                         (write! close))))]
      (try
        (letfn [(w [x depth]
                  (cond
                    (outline? x) (write-outline x write! #(w % depth) length)
                    (instance? JsLeaf x) (do (write! (.-text ^JsLeaf x))
                                             (when-not (.-whole ^JsLeaf x) (vreset! left-out true)))
                    (instance? JsColl x)
                    (let [^JsColl c x]
                      (if (= :seq (.-shape c))
                        (let [loaded @(.-loaded c)
                              n (.-count c)]
                          ;; the parts not sent print as ...
                          (items! w "#array[" (map #(if-let [[_ v] (get loaded %)] v gone)
                                                   (range (min n (inc (or length n)))))
                                  "]" depth))
                        (do (when (pos? (.-count c)) (vreset! hidden true))
                            (write! (.-text c)))))
                    (array? x) (items! w "#array[" (seq x) "]" depth)
                    :else (do (when (or (datafiable? x) (and (instance? Var x) (.isBound ^Var x)))
                                (vreset! hidden true))
                              (write! (Printer/print x)))))]
          (w x 0))
        [(str sb) (not @left-out) @hidden]
        (catch clojure.lang.ExceptionInfo e
          (if (::full (ex-data e))
            [(subs (str sb) 0 limit) false @hidden]
            (throw e)))
        (catch Throwable t
          [(str "#<unprintable " (or (ex-message t) (.getName (class t))) ">") true false])))))

(defn- summary
  "`x` as one line of at most `width` chars: {:text :whole :cut}. Printed
  whole where it fits; where it does not, short, the first eight of
  everything, as many levels down as fit, and fewer than eight where one
  level of eight does not; and where even that does not fit, cut."
  [x width]
  (let [[text whole? hidden?] (if (> (or (when (shape x) (counted x)) 0) width)
                                ;; a char each at least: it does not fit
                                [nil false]
                                (print-within x width nil nil))
        one-line (fn [s] (str/replace s #"\s*\n\s*" " "))]
    (if whole?
      ;; all of it, but what it holds that its printing does not show
      {:text (one-line text) :whole (not hidden?)}
      (let [fits (fn [length level]
                   (let [[text whole?] (print-within x width length level)]
                     (when whole? (one-line text))))]
        (if-let [text (fits 8 1)]
          {:text (loop [level 2 text text]
                   (if-let [deeper (when (<= level 4) (fits 8 level))]
                     (recur (inc level) deeper)
                     text))
           :whole false}
          (or (some (fn [length] (some->> (fits length 1) (array-map :whole false :text)))
                    [4 2 1])
              {:text (one-line (first (print-within x width 8 1))) :whole false :cut true}))))))

;; --- finding a child ----------------------------------------------------------

(defn- lookup
  "What `step` leads to from a node whose children are taken from `b`, or
  `gone`."
  [b [via k]]
  (case via
    :index (cond
             (instance? JsColl b) (js-part b k)
             (array? b) (if (< -1 k (count b)) (nth b k) gone)
             :else gone)
    :deref (cond
             (instance? JsColl b) (js-part b "@")
             (and (instance? Var b) (.isBound ^Var b)) (.deref ^Var b)
             :else gone)
    :prop (cond
            (instance? JsColl b) (js-part b k)
            (instance? Props b) (if-let [[v] (some (fn [[key v]] (when (= k key) [v])) (.-entries ^Props b))]
                                  v
                                  gone)
            :else gone)
    :label (if (outline? b) (outline-get b k) gone)
    gone))

;; --- the view -------------------------------------------------------------------

(defn view
  "A view whose root holds `root`. What the functions below take, in an
  atom."
  [root]
  {:gen 0
   :nodes {0 {:raw root :gen 0 :value root :browse (browsed root) :valued 0}}
   :index {}
   :next 1})

(defn- same?
  "Whether `a` is what `b` was: a JVM value that holds things by identity,
  a JS one by its identity in Node and how many things it holds, an
  outline by what is under each label, anything else by value."
  [a b]
  (cond
    (outline? a) (and (outline? b)
                      (let [ea (entries a) eb (entries b)]
                        (and (= (count ea) (count eb))
                             (every? (fn [[[la va] [lb vb]]] (and (= la lb) (same? va vb)))
                                     (map vector ea eb)))))
    (instance? JsColl a) (and (instance? JsColl b)
                              (= (.-id ^JsColl a) (.-id ^JsColl b))
                              (= (.-count ^JsColl a) (.-count ^JsColl b)))
    (instance? JsLeaf a) (and (instance? JsLeaf b)
                              (= (.-text ^JsLeaf a) (.-text ^JsLeaf b))
                              (= (.-kind ^JsLeaf a) (.-kind ^JsLeaf b)))
    (shape a) (identical? a b)
    :else (= a b)))

(defn- seen
  "`rec`, the node found by its path to hold `raw` in generation `gen`.
  Whether it changed is decided the first time a generation sees it."
  [rec gen raw]
  (if (= gen (:gen rec))
    rec
    (assoc rec
           :gen gen
           :raw raw
           :changed (and (contains? rec :raw) (not (same? raw (:raw rec)))))))

(defn root!
  "Put `root` at the root of `view`, a new generation, in which every node
  is found again from it."
  [view root]
  (swap! view
         (fn [{:keys [gen nodes] :as v}]
           (let [gen (inc gen)]
             (-> v
                 (assoc :gen gen)
                 (assoc-in [:nodes 0]
                           (-> (seen (get nodes 0) gen root)
                               (assoc :value root :browse (browsed root) :valued gen))))))))

(defn- current
  "The node `id` of `view` as of its generation, with its :value and
  :browse, or nil where its path no longer leads anywhere."
  [view id]
  (let [{:keys [gen nodes]} @view
        rec (get nodes id)]
    (cond
      (nil? rec) nil
      (= gen (:valued rec)) rec
      :else
      (when-let [parent (current view (:parent rec))]
        (let [raw (lookup (:browse parent) (:step rec))]
          (when-not (identical? gone raw)
            (let [rec (-> (seen rec gen raw)
                          (assoc :value raw :browse (browsed raw) :valued gen))]
              (swap! view assoc-in [:nodes id] rec)
              rec)))))))

(defn value
  "What the node `id` of `view` holds now, or `gone`."
  [view id]
  (if-let [rec (current view id)] (:value rec) gone))

(defn- node!
  "The id of the child of `parent` found by `step` to hold `raw`, made the
  first time it is asked for: the same path is the same id."
  [view parent step raw]
  (let [made (volatile! nil)]
    (swap! view
           (fn [{:keys [gen index next] :as v}]
             (if-let [id (get index [parent step])]
               (do (vreset! made id)
                   (update-in v [:nodes id] seen gen raw))
               (do (vreset! made next)
                   (-> v
                       (assoc-in [:index [parent step]] next)
                       (assoc-in [:nodes next] (seen {:parent parent :step step} gen raw))
                       (assoc :next (inc next)))))))
    @made))

;; --- what an editor is shown -------------------------------------------------------

(defn- page-of
  "The children of a node whose children are taken from `b`: the `limit`
  of them from `offset`, as [step raw] pairs, one more than asked for where
  there is one, which is how it is known that there are more."
  [b offset limit]
  (let [n (inc limit)]
    (case (shape b)
      :seq (if (instance? JsColl b)
             (map (fn [[k v]] [[:index k] v]) (js-parts b offset (+ offset n)))
             (for [i (range offset (min (count b) (+ offset n)))]
               [[:index i] (nth b i)]))
      :ref (when (zero? offset)
             (if (instance? JsColl b)
               (map (fn [[_ v]] [[:deref] v]) (js-parts b 0 1))
               (when (.isBound ^Var b) [[[:deref] (.deref ^Var b)]])))
      :object (map (fn [[k v]] [[:prop k] v])
                   (if (instance? Props b)
                     (->> (.-entries ^Props b) (drop offset) (take n))
                     (js-parts b offset (+ offset n))))
      :outline (->> (entries b) (drop offset) (take n) (map (fn [[label v]] [[:label label] v])))
      nil)))

(defn- label
  "What the line of the child found by `step` is labelled with."
  [[via k]]
  (case via
    :index {:via "index" :key (str k)}
    :deref {:via "deref" :key "@"}
    :prop {:via "prop" :key (str k)}
    :label {:via "label" :key k}))

(defn- expandable?
  "Whether a line holding `x` has more in it than it shows: what does not
  fit on it, what a var holds, a JS object's properties, an exception's
  message, data and cause."
  [x whole?]
  (or (case (shape x)
        :ref (if (instance? Var x) (.isBound ^Var x) (pos? (counted x)))
        :object (pos? (counted x))
        (:seq :outline) (and (not whole?) (not (zero? (or (counted x) 1))))
        false)
      (datafiable? x)))

(def ^:private margin
  "What is kept free at the end of a line, for the delimiters that close
  after it and the space between two entries."
  2)

(defn- value-column
  "The column the value of a child labelled `label` is written at, its
  entry starting at `column`: after its key, where it has one."
  [column {:keys [via key pad above]}]
  (+ column (case via
              "prop" (inc (count key))
              ;; the labels of an outline are padded to the longest, or
              ;; the value is on the line under its label, indented
              "label" (if above 2 (inc (max (count key) (or pad 0))))
              0)))

(defn- describe
  "The line showing `x`, the child of `parent` found by `step`, or the
  root, where `step` is nil, its entry written at `column` on lines of
  `width` chars."
  [view parent b step x {width :width column :column pad :pad}]
  (let [width (or width 80)
        column (or column 0)
        label (when step (cond-> (label step)
                           pad (assoc :pad pad)
                           (and (outline? b) (above? b (second step))) (assoc :above true)))
        {:keys [text whole cut]} (summary x (max 20 (- width (value-column column label)
                                                   (if step margin 0))))
        ;; a node for every line, so that a line can say it changed
        id (if (nil? step) 0 (node! view parent step x))
        ;; the root is open whatever it holds: it is what the view is of
        expandable (expandable? x (and whole (some? step)))
        rec (get-in @view [:nodes id])
        n (when (shape x) (counted x))]
    (cond-> {:value text :kind (kind x)}
      (type-name x) (assoc :type (type-name x))
      n (assoc :count n)
      (not whole) (assoc :truncated true)
      cut (assoc :cut true)
      true (assoc :node id)
      expandable (assoc :expandable true)
      (:changed rec) (assoc :changed true)
      label (merge label))))

(defn- delimiters
  "What a node holding `v`, whose children are taken from `b`, is written
  between once it is open: [open close], the way it prints."
  [v b]
  (let [[open close]
        (case (shape b)
          :seq ["#array[" "]"]
          :ref [(str (if (instance? JsColl b) (.-text ^JsColl b) (Printer/print b)) " ") ""]
          :object (let [t (type-name v)] [(if (= "Object" t) "#js {" (str "#object[" t "] {")) "}"])
          ;; an outline is a heading a line, between nothing
          ["" ""])]
    [open close]))

(defn- children-page
  "A page of the children of the node `id` of `view`, written at `column`,
  or nil where the node is no longer there. The children of an outline are
  labelled, and their labels padded to the longest of the page, :pad."
  [view id {offset :offset limit :limit column :column :or {offset 0 limit 100 column 0} :as opts}]
  (when-let [{v :value b :browse} (current view id)]
    (let [[open close] (delimiters v b)
          page (page-of b offset limit)
          more? (> (count page) limit)
          page (take limit page)
          opts (if (outline? b)
                 (assoc opts :column column
                        :pad (reduce max 0 (keep (fn [[[_ label]]]
                                                   (when-not (above? b label) (count label)))
                                                 page)))
                 (-> opts (assoc :column (+ column (count open))) (dissoc :pad)))]
      (cond-> {:children (mapv (fn [[step x]] (describe view id b step x opts)) page)
               :more more?
               :open open
               :close close}
        (counted b) (assoc :total (counted b))
        (= :ref (shape b)) (assoc :total (count page))))))

;; --- opened as far as a screen goes -------------------------------------------------

(defn- keyed? [line]
  (contains? #{"prop" "label"} (:via line)))

(defn- lines-of
  "About how many lines `page` takes once laid out, its entries starting at
  `base` on lines of `width` chars: an entry with a key a line, and so is
  an element that holds things, and what holds none filled into as few
  lines as it takes. And a line for saying there is more."
  [{:keys [children more]} base width]
  (+ (if (some #(or (keyed? %) (:count %)) children)
       (+ (count children) (count (filter :above children)))
       (let [w (max 1 (- width base))]
         (quot (+ (reduce + 0 (map #(inc (count (:value %))) children)) (dec w)) w)))
     (if more 1 0)))

(defn- within
  "`page` cut to the first of its children that take no more than `left`
  lines."
  [page base width left]
  (loop [page page]
    (if (and (> (lines-of page base width) left) (> (count (:children page)) 1))
      (recur (-> page (update :children pop) (assoc :more true)))
      page)))

(defn- expanded
  "`page`, the children of the node at `column`, with children of its own
  opened in it, each as its :page, as long as what is shown stays within
  `budget` lines. Breadth first, and only what does not fit on its line."
  [view page column {:keys [width limit] :or {width 80 limit 100} :as opts} budget]
  (let [base (fn [page column] (+ column (count (:open page))))
        candidates (fn [page column]
                     (for [line (:children page)
                           :when (and (:expandable line) (:truncated line))]
                       [line (if (= "outline" (:kind line))
                               ;; an outline in an outline is under its label
                               (+ (base page column) 2)
                               (value-column (base page column) line))]))
        opened (volatile! {})]
    (loop [queue (into clojure.lang.PersistentQueue/EMPTY (candidates page column))
           used (lines-of page (base page column) width)]
      (let [[line col] (peek queue)
            left (- budget used)]
        (when (and line (> left 2))
          (let [;; a share of what is left, so that what comes after it on
                ;; the same level is opened too
                left (min left (max 8 (quot left (count queue))))
                keyed (contains? #{"object" "outline"} (:kind line))
                p (children-page view (:node line)
                                 (assoc opts :column col :offset 0
                                        :limit (min limit (if keyed (dec left) (* 4 left)))))
                p (some-> p (within (base p col) width (inc left)))]
            (if (or (nil? p)
                    ;; opened, it would show less than it does closed
                    (and (:more p) (< (count (:children p)) 8)))
              (recur (pop queue) used)
              (do (vswap! opened assoc (:node line) p)
                  (recur (into (pop queue) (candidates p col))
                         (+ used (dec (lines-of p (base p col) width))))))))))
    (letfn [(assemble [page]
              (update page :children
                      (fn [lines]
                        (mapv (fn [line]
                                (if-let [p (get @opened (:node line))]
                                  (assoc line :page (assemble p))
                                  line))
                              lines))))]
      (assemble page))))

(defn children
  "A page of the children of the node `id` of `view`: {:children [line
  ...] :total n :more bool :open :close}, or {:gone true} where the node is
  no longer there. `opts` are :offset and :limit, and where the node is
  written: at :column, on lines of :width chars. With :expand, the children
  are opened in turn as far as that many lines."
  [view id {column :column budget :expand :or {column 0} :as opts}]
  (if-let [p (children-page view id opts)]
    (if (and budget (pos? budget))
      (expanded view p column opts budget)
      p)
    {:gone true}))

(defn printed
  "The node `id` of `view` printed whole, up to `limit` chars: {:value
  :whole}, or {:gone true}. A value of Node is printed there."
  [view id limit]
  (if-let [{v :value} (current view id)]
    (let [[text whole?] (if (or (instance? JsColl v) (instance? JsLeaf v))
                          (js-printed v limit)
                          (print-within v limit nil nil))]
      {:value text :whole whole?})
    {:gone true}))

(defn js?
  "Whether `x` is a value of Node."
  [x]
  (or (instance? JsLeaf x) (instance? JsColl x)))

;; --- a session ------------------------------------------------------------------------

(defn session
  "A view of what `top`, a function of nothing, answers, asked again at
  each refresh."
  [top]
  {:top top :model (atom (view (top)))})

(defn refresh!
  "Put what the session's `top` answers now at its root."
  [{:keys [top model]}]
  (root! model (top)))

(defn shown
  "What an editor is shown of the session: the root's line, and the first
  page of its children where there is more in it than the line shows."
  [{:keys [model]} opts]
  (let [root (describe model nil nil nil (:value (get-in @model [:nodes 0])) opts)]
    (cond-> {:root root}
      (:expandable root) (merge (children model 0 opts)))))

(defn node-values
  "The values of the Node of `rt` that `places` say, [root path]s, as
  mirrors: each a JsLeaf, a JsColl or nil, or `gone` where it leads
  nowhere. A root is [\"call\" c site number], a value kept of a call, or
  [\"var\" namespace property], what a var holds."
  [rt places]
  (if (empty? places)
    []
    (let [ms (some-> (recording/in-node rt "mirror"
                                        (js-array (map (fn [[root path]] (js-array [(js-root root) (js-path path)]))
                                                       places)))
                     node-json)]
      (mapv (fn [[root path] m] (if (some? m) (js-value {:rt rt :root root :path (vec path)} m) gone))
            places
            (concat ms (repeat nil))))))
