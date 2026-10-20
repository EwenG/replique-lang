(ns replique.reader.cst
  "The lossless reader: source text to a concrete syntax tree (doc/reader.md §2).

  Every character of the source belongs to exactly one leaf, whitespace and
  comments included, so (text node) of the nodes of a parse gives the source
  back. Nothing here knows about namespaces, targets or values: symbols are
  not resolved, numbers are not converted, reader conditionals keep every
  branch. That is the job of replique.reader.forms.

  A node is a map:

    leaf       {:kind k :text \"...\" :start s :end e}
    container  {:kind k :open \"(\" :close \")\" :children [...] :start s :end e}
    prefix     {:kind k :open \"'\" :close nil :children [...] :start s :end e}

  (all of them Node records, so lookups are fast, with nil for the keys a
  kind doesn't use; compare their fields rather than comparing them to maps.)

  Offsets are indexes into the source string (UTF-16 code units, as Java
  counts them), the end exclusive. A container's :close is nil when the source
  ended before it. A node that contains an error, at any depth, has :error? true,
  and the error itself is in the parse's :diagnostics.

  Kinds:

    trivia     :whitespace (commas included) :comment
    atoms      :symbol :keyword :number :string :char :regex :symbolic (##Inf)
    containers :list :vector :map :set :fn (#(...)) :reader-cond (#?(...) and #?@(...))
    prefix     :quote :syntax-quote :unquote :unquote-splicing :deref :var
               :discard (#_) :meta (^, two forms) :tagged (#inst, the tag in :tag)
               :namespaced-map (#:ns{...}, the prefix in :open)
    errors     :error, a leaf holding text that could not be read; :read-eval,
               #= and the form after it, which is always an error"
  (:require [clojure.string :as str]))

;; --- characters ------------------------------------------------------------

(defn whitespace? [c]
  (and c (let [c (char c)] (or (= \, c) (Character/isWhitespace c)))))

(defmacro ^:private ws-char?
  "Whether the primitive char `c` is whitespace; commas are."
  [c]
  `(let [c# ~c] (or (= \, c#) (= \space c#) (= \newline c#) (Character/isWhitespace c#))))

(defmacro ^:private terminating-char?
  "Whether `c` ends a token: Clojure's macro characters except # ' and %,
  which may appear inside a symbol."
  [c]
  `(case ~c (\" \; \@ \^ \` \~ \( \) \[ \] \{ \} \\) true false))

(defn- token-char? [c]
  (and c (let [c (char c)] (not (or (ws-char? c) (terminating-char? c))))))

(def ^:private closers {\( \) \[ \] \{ \}})

(def ^:private container-kind {\( :list \[ :vector \{ :map})

;; --- source positions ------------------------------------------------------

(defn line-starts
  "The offset at which each line of `src` starts, as a long array."
  ^longs [^String src]
  (let [starts (java.util.ArrayList.)]
    (.add starts 0)
    (loop [i 0]
      (let [j (.indexOf src "\n" (int i))]
        (when-not (neg? j)
          (.add starts (inc j))
          (recur (inc j)))))
    (long-array starts)))

(defn- line-index
  "The index of the line holding `offset`: the last line starting at or before
  it."
  ^long [^longs starts ^long offset]
  (let [i (java.util.Arrays/binarySearch starts offset)]
    (if (neg? i) (- (- i) 2) i)))

(defn- line-index-near
  "line-index, trying line `hint` and the next one first: spans are mostly
  asked for in source order."
  ^long [^longs starts ^long hint ^long offset]
  (let [n (alength starts)]
    (cond
      (and (< hint n) (<= (aget starts hint) offset)
           (or (= (inc hint) n) (< offset (aget starts (inc hint)))))
      hint
      (and (< (inc hint) n) (<= (aget starts (inc hint)) offset)
           (or (= (+ 2 hint) n) (< offset (aget starts (+ 2 hint)))))
      (inc hint)
      :else (line-index starts offset))))

(defn position
  "[line column] of `offset`, both 1-based."
  [^longs starts offset]
  (let [i (line-index starts offset)]
    [(inc i) (inc (- (long offset) (aget starts i)))]))

(defn span
  "The span of offsets [start end) in a parse, as doc/ir.md §1 describes it."
  ([parse node] (span parse (:start node) (:end node)))
  ([parse start end]
   (let [^longs lines (:lines parse)
         ^longs hint (:line-hint parse)
         start (long start)
         end (long end)
         l (line-index-near lines (if hint (aget hint 0) 0) start)
         el (line-index-near lines l end)
         _ (when hint (aset hint 0 l))
         file (:file parse)
         m {:line (inc l) :column (inc (- start (aget lines l)))
            :end-line (inc el) :end-column (inc (- end (aget lines el)))}]
     (if file (assoc m :file file) m))))

;; --- the parser ------------------------------------------------------------

;; A record rather than maps, for the speed of its keyword lookups, and one
;; record for leaves and the rest, so those lookups stay monomorphic. It is
;; still a map; the optional keys (:error? :splicing? :tag) are assoc'ed.
(defrecord Node [kind text open close children start end])

(deftype ^:private State [^String src ^longs cursor diags]
  ;; the source, the cursor (a one-element array) and the diagnostics so far
  clojure.lang.IDeref
  (deref [_] (aget cursor 0)))

(defn- peek-char
  ([^State st] (peek-char st 0))
  ([^State st ahead]
   (let [^String src (.-src st) i (+ (long @st) (long ahead))]
     (when (< i (.length src)) (.charAt src i)))))

(defn- advance! [^State st n]
  (let [^longs c (.-cursor st)]
    (aset c 0 (+ (aget c 0) (long n)))))

(defn- emit-diag! [^State st diag]
  (vswap! (.-diags st) conj diag))

(defn- diag
  "A diagnostic, with offsets; replique.reader turns them into spans."
  [code start end message & {:as more}]
  (merge {:severity :error :code code :message message :start start :end end}
         more))

(defn- leaf [^State st kind start]
  (->Node kind (subs (.-src st) start @st) nil nil nil start @st))

(defn- error-leaf [^State st start code message & more]
  (emit-diag! st (apply diag code start @st message more))
  (assoc (leaf st :error start) :error? true))

(defmacro ^:private scan!
  "Advance the cursor of `st` while the char at it satisfies `test`, an
  expression of `c`: the hot loops, without boxing a character per step."
  [st c test]
  (let [state (with-meta (gensym "st") {:tag `State})
        src (with-meta (gensym "src") {:tag 'String})
        cur (with-meta (gensym "cur") {:tag 'longs})
        i (gensym "i")]
    `(let [~state ~st
           ~src (.-src ~state)
           n# (.length ~src)
           ~cur (.-cursor ~state)]
       (loop [~i (aget ~cur 0)]
         (if (and (< ~i n#) (let [~c (.charAt ~src (int ~i))] ~test))
           (recur (inc ~i))
           (aset ~cur 0 ~i))))))

(defn- skip-whitespace! [st] (scan! st c (ws-char? c)))
(defn- skip-line! [st] (scan! st c (not= \newline c)))
(defn- skip-token! [st] (scan! st c (not (or (ws-char? c) (terminating-char? c)))))

(declare read-node)

(defn- trivia? [node] (case (:kind node) (:whitespace :comment) true false))

(defn- with-errors
  "`node` with :error? set if any child has it."
  [node]
  (if (some :error? (:children node)) (assoc node :error? true) node))

(defn- read-container
  "A container whose opening text is `open` (already consumed from `start`)."
  [^State st kind open start]
  (let [close (closers (last open))]
    (loop [children (transient []) error? false]
      (let [c (peek-char st)]
        (cond
          (nil? c)
          (do (emit-diag! st (diag :reader/eof start @st
                                   (str "the source ended inside the " (name kind)
                                        " opened here; expected " close)
                                   :incomplete? true))
              (assoc (->Node kind nil open nil (persistent! children) start @st) :error? true))

          (= close c)
          (do (advance! st 1)
              (cond-> (->Node kind nil open (str close) (persistent! children) start @st)
                error? (assoc :error? true)))

          :else
          (let [node (read-node st)]
            (recur (conj! children node) (or error? (boolean (:error? node))))))))))

(defn- read-prefixed
  "A prefix node: `open` (consumed from `start`), then trivia, then `n` forms."
  [^State st kind open start n & {:as extra}]
  (loop [children [] needed n]
    (if (zero? needed)
      (with-errors (merge (->Node kind nil open nil children start @st) extra))
      (let [c (peek-char st)]
        (if (or (nil? c) (#{\) \] \}} c))
          (do (emit-diag! st (diag :reader/missing-form start @st
                                   (str "expected a form after " open)
                                   :incomplete? (nil? c)))
              (merge (assoc (->Node kind nil open nil children start @st) :error? true) extra))
          (let [node (read-node st)]
            (recur (conj children node)
                   (if (or (trivia? node) (= :discard (:kind node))) needed (dec needed)))))))))

(defn- read-string-body
  "From just after an opening quote to just after the closing one. Returns
  false if the source ended first. Escapes are checked by the forms phase."
  [^State st]
  (loop []
    (let [c (peek-char st)]
      (cond
        (nil? c) false
        (= \\ c) (do (advance! st (if (peek-char st 1) 2 1)) (recur))
        (= \" c) (do (advance! st 1) true)
        :else (do (advance! st 1) (recur))))))

(defn- read-string-like [^State st kind start]
  (if (read-string-body st)
    (leaf st kind start)
    (error-leaf st start :reader/eof "the source ended inside a string" :incomplete? true)))

(def int-pattern
  #"([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?")
(def ratio-pattern #"([-+]?[0-9]+)/([0-9]+)")
(def float-pattern #"([-+]?[0-9]+(\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?")

(def ^:private symbol-pattern
  ;; Clojure's: an optional namespace, then a name; neither starts with a digit,
  ;; and / alone, or as the name, is allowed
  #"[:]?([\D&&[^/]].*/)?(/|[\D&&[^/]][^/]*)")

(defn number-token? [^String s]
  (boolean (or (re-matches int-pattern s) (re-matches ratio-pattern s)
               (re-matches float-pattern s))))

(defn- valid-symbol?
  "Clojure's rules: the pattern, and no name ending in : or holding ::. A
  keyword is checked with one leading colon."
  [^String s]
  (if (and (neg? (.indexOf s "/")) (neg? (.indexOf s ":" 1)) (not (str/starts-with? s ":")))
    ;; the common case, which the pattern accepts whenever the first character
    ;; isn't a digit, and read-token sent digits elsewhere
    true
    (boolean (and (re-matches symbol-pattern s)
                  (not (str/ends-with? s ":"))
                  (not (str/includes? (subs s 1) "::"))))))

(defn- read-token [^State st start]
  (skip-token! st)
  (let [text (subs (.-src st) start @st)
        c0 (.charAt text 0)
        c1 (when (> (count text) 1) (.charAt text 1))]
    (cond
      (or (Character/isDigit c0)
          (and (#{\+ \-} c0) c1 (Character/isDigit (char c1))))
      (if (number-token? text)
        (leaf st :number start)
        (error-leaf st start :reader/invalid-number (str "invalid number " text)))

      (= \: c0)
      (if (and (> (count text) 1)
               (valid-symbol? (if (str/starts-with? text "::") (subs text 1) text))
               (not (str/starts-with? text ":::")))
        (leaf st :keyword start)
        (error-leaf st start :reader/invalid-token (str "invalid keyword " text)))

      (valid-symbol? text) (leaf st :symbol start)

      :else (error-leaf st start :reader/invalid-token (str "invalid token " text)))))

(defn- read-char-literal [^State st start]
  ;; a backslash, then one character whatever it is, then any token characters
  (advance! st 1)
  (if (nil? (peek-char st))
    (error-leaf st start :reader/eof "the source ended after a backslash" :incomplete? true)
    (do (advance! st 1)
        (skip-token! st)
        (leaf st :char start))))

(defn- read-dispatch
  "After #, at `start`."
  [^State st start]
  (advance! st 1)
  (let [c (peek-char st)]
    (case c
      \{ (do (advance! st 1) (read-container st :set "#{" start))
      \( (do (advance! st 1) (read-container st :fn "#(" start))
      \" (do (advance! st 1) (read-string-like st :regex start))
      \' (do (advance! st 1) (read-prefixed st :var "#'" start 1))
      \_ (do (advance! st 1) (read-prefixed st :discard "#_" start 1))
      \! (do (skip-line! st) (leaf st :comment start))
      \# (do (advance! st 1) (skip-token! st) (leaf st :symbolic start))
      \? (let [splicing? (= \@ (peek-char st 1))
               open (if splicing? "#?@(" "#?(")]
           (if (= \( (peek-char st (if splicing? 2 1)))
             (do (advance! st (if splicing? 3 2))
                 (assoc (read-container st :reader-cond open start) :splicing? splicing?))
             (do (advance! st 1)
                 (error-leaf st start :reader/invalid-reader-cond
                             "a reader conditional is #?( or #?@( followed by its branches"))))
      \: (do (skip-token! st)
             (let [open (subs (.-src st) start @st)]
               (if (re-matches #"#:[^:/]+|#::([^:/]+)?" open)
                 (read-prefixed st :namespaced-map open start 1)
                 (error-leaf st start :reader/invalid-token
                             (str "invalid namespaced map prefix " open)))))
      \= (do (advance! st 1)
             ;; the form after it is read too, so that it doesn't turn up as a
             ;; top-level form of its own
             (emit-diag! st (diag :reader/read-eval start @st
                                  "#= is not part of Replique: the reader never evaluates code"))
             (assoc (read-prefixed st :read-eval "#=" start 1) :error? true))
      \< (do (skip-token! st)
             (error-leaf st start :reader/unreadable "#< marks an unreadable value"))
      (if (and c (token-char? c) (Character/isLetter (char c)))
        (do (skip-token! st)
            (let [open (subs (.-src st) start @st)]
              (read-prefixed st :tagged open start 1 :tag (symbol (subs open 1)))))
        (do (when c (advance! st 1))
            (error-leaf st start :reader/unknown-dispatch
                        (str "no reader macro #" (or c ""))
                        :incomplete? (nil? c)))))))

(defn- read-node
  "One node, at the cursor, which is not at the end of the source."
  [^State st]
  (let [start @st
        c (peek-char st)]
    (cond
      (whitespace? c) (do (skip-whitespace! st) (leaf st :whitespace start))
      (= \; c) (do (skip-line! st) (leaf st :comment start))
      (container-kind c) (do (advance! st 1) (read-container st (container-kind c) (str c) start))
      (#{\) \] \}} c) (do (advance! st 1)
                          (error-leaf st start :reader/unmatched-delimiter
                                      (str "unmatched delimiter " c)))
      (= \" c) (do (advance! st 1) (read-string-like st :string start))
      (= \\ c) (read-char-literal st start)
      (= \' c) (do (advance! st 1) (read-prefixed st :quote "'" start 1))
      (= \` c) (do (advance! st 1) (read-prefixed st :syntax-quote "`" start 1))
      (= \~ c) (if (= \@ (peek-char st 1))
                 (do (advance! st 2) (read-prefixed st :unquote-splicing "~@" start 1))
                 (do (advance! st 1) (read-prefixed st :unquote "~" start 1)))
      (= \@ c) (do (advance! st 1) (read-prefixed st :deref "@" start 1))
      (= \^ c) (do (advance! st 1) (read-prefixed st :meta "^" start 2))
      (= \# c) (read-dispatch st start)
      :else (read-token st start))))

(defn parse
  "The CST of `src`: {:nodes [...] :diagnostics [...] :lines [...] :source src
  :file file}. Never throws on bad input: errors are diagnostics, and the
  nodes still cover the whole source."
  ([src] (parse src nil))
  ([^String src {:keys [file]}]
   (let [st (State. src (long-array 1) (volatile! []))
         nodes (loop [nodes []]
                 (if (peek-char st)
                   (recur (conj nodes (read-node st)))
                   nodes))]
     {:nodes nodes
      :diagnostics @(.-diags st)
      :lines (line-starts src)
      :line-hint (long-array 1)
      :source src
      :file file})))

(defn source-text
  "The source text of `node`, cut from the parse's source: what `text` gives,
  without rebuilding it."
  [parse node]
  (subs (:source parse) (:start node) (:end node)))

(defn text
  "The source text of `node`, rebuilt from the tree."
  [node]
  (if-let [t (:text node)]
    t
    (str (:open node) (apply str (map text (:children node))) (:close node))))

(defn forms
  "The children of `node` that are forms: not trivia, not discarded."
  [node]
  (remove #(or (trivia? %) (= :discard (:kind %))) (:children node)))

(defn incomplete?
  "Whether the parse failed only because the source ended too early, which is
  what a REPL needs to know to ask for another line."
  [{:keys [diagnostics]}]
  (and (seq diagnostics) (every? :incomplete? diagnostics)))
