(ns replique.tooling.iterations
  "Which of the values a call recorded are those of one iteration of a loop
  of it (doc/server.md §5), the same of a call kept on the JVM and of one
  kept in Node. Ported from replique's replique.iterations, without what
  makes outlines of them, which the inspector's views will.

  Every value a call records is numbered, in the order it was recorded in.
  A name a loop binds being bound starts an iteration of the loop, and the
  iteration goes on until it is bound again, or until the loop it is in, for
  one in another, goes round again. What the code the loop evaluates again
  each time round evaluated to in between is that iteration's: once, or
  more, or not at all, a branch not taken.

  Of a form, the first values and the last ones are kept, numbered: what is
  kept of it is {:times n :head [[number value] ...] :tail [...]}, the first
  values and the last, the two the same where it was evaluated as many
  times as are kept, or fewer. An iteration in between is not kept, and nor
  are the values of a form evaluated more times than are kept in it, where
  the ones not kept may be among them.

  The value of an entry may be anything: the server takes its number, and
  asks for the value of a number once it knows which it shows.")

(defn- number [entry] (nth entry 0))

(defn- not-kept
  "The numbers of the values of `kept` not kept being between them, [after
  before], or nil where none was not kept."
  [{:keys [times head tail]}]
  (let [upto (number (peek head))
        both (count (filter #(<= (number %) upto) tail))]
    (when (< (- (+ (count head) (count tail)) both) times)
      [upto (number (nth tail 0))])))

(defn- may-miss?
  "Whether a value of `kept` not kept may be numbered `from` or after,
  before `to`, nil for no end."
  [kept from to]
  (when-let [[after before] (not-kept kept)]
    (< (max from (inc after)) (if to (min to before) before))))

(defn- entries
  "The values of `kept`, numbered, in order, each once."
  [{:keys [head tail]}]
  (let [upto (number (peek head))]
    (into head (drop-while #(<= (number %) upto)) tail)))

(defn- nth-number
  "The number of the `k`th value of `kept`, from 1, or nil where it is not
  kept."
  [{:keys [times head tail]} k]
  (cond
    (<= 1 k (count head)) (number (nth head (dec k)))
    (< (- times (count tail)) k (inc times)) (number (nth tail (- k (- times (count tail)) 1)))))

(defn events
  "The values kept of the sites `sites` of a call, `kept-of` saying what is
  kept of each, in the order they were recorded: [number site] pairs."
  [kept-of sites]
  (->> sites
       (mapcat (fn [site] (map (fn [e] [(number e) site]) (some-> (kept-of site) entries))))
       (sort-by first)
       vec))

(defn numbered
  "The value of `kept` numbered `n`, {:value v}, or nil where it is not
  kept."
  [kept n]
  (some (fn [e] (when (= n (number e)) {:value (nth e 1)})) (some-> kept entries)))

(defn latest
  "Of `kept`, the last value numbered `n` or before it: [number value], nil
  where there is none, and ::not-kept where one not kept may be."
  [kept n]
  (when kept
    (let [e (last (take-while #(<= (number %) n) (entries kept)))]
      (cond
        (may-miss? kept (if e (inc (number e)) 1) (inc n)) ::not-kept
        e e))))

(defn kept-iterations
  "Which iterations of a loop whose name was bound `times` times are kept,
  where as many as `kept` values of a form are kept first and as many last:
  [first last] ranges of them, from 1, an iteration being kept where its
  start is, and its end, the next one's start."
  [times kept]
  (if (<= times (* 2 kept))
    (if (pos? times) [[1 times]] [])
    [[1 (dec kept)] [(inc (- times kept)) times]]))

(defn- first-bound
  "Of the sites of the names a loop binds each time round, `sites`, the one
  bound first each time round, the one whose first value is numbered
  lowest, or nil where none is kept."
  [kept-of sites]
  (some->> (seq (keep (fn [site]
                        (when-let [entry (first (:head (kept-of site)))]
                          [(number entry) site]))
                      sites))
           (apply min-key first)
           second))

(defn times-round
  "How many times a loop went round, whose names are bound each time round
  at the sites `sites`, or nil where none of them was bound."
  [kept-of sites]
  (some-> (first-bound kept-of sites) kept-of :times))

(defn index-at
  "The index, from 1, of the iteration the value numbered `n` is of, of a
  loop whose names are bound each time round at the sites `sites`, 0 before
  the first one, or nil where that is not kept: the iterations around `n`
  were not."
  [kept-of sites n]
  (when-let [kept (some-> (first-bound kept-of sites) kept-of)]
    (let [[after before :as gap] (not-kept kept)
          upto (fn [es] (count (take-while #(<= (number %) n) es)))]
      (cond
        (nil? gap) (upto (entries kept))
        (<= n after) (upto (:head kept))
        (>= n before) (- (:times kept) (count (drop-while #(<= (number %) n) (:tail kept))))))))

(defn ranges
  "The numbers of the values of the iteration `index`, from 1, of the loop
  whose name was bound at the site `loop`, from the first one to the one
  just after the last, or nil, for the last iteration of a loop in nothing,
  and of the iteration of each loop of `enclosing` it is in, the sites
  their names were bound at: {site [from to]}. `kept-of` answers what is
  kept of a site; `starts`, a map, the sites of all the names each loop
  binds each time round, of which the one bound first starts an iteration.

  Nil where that is not kept."
  [kept-of {:keys [loop index enclosing starts]}]
  (let [kept-of-loop (fn [site] (kept-of (or (first-bound kept-of (get starts site)) site)))
        kept (kept-of-loop loop)
        from (when kept (nth-number kept index))
        last? (and kept (= index (:times kept)))
        to (when (and kept (not last?)) (nth-number kept (inc index)))
        around (into {}
                     (map (fn [site]
                            (let [kept (kept-of-loop site)
                                  es (when kept (entries kept))
                                  start (some-> (last (take-while #(<= (number %) from) es)) number)
                                  end (some-> (first (drop-while #(<= (number %) from) es)) number)]
                              [site (when (and start (not (may-miss? kept (inc start) end)))
                                      [start end])])))
                     (when from enclosing))]
    (when (and from (or to last?) (every? some? (vals around)))
      (let [ends (keep second (cons [from to] (vals around)))]
        (assoc around loop [from (when (seq ends) (apply min ends))])))))

(defn value-in
  "What is kept of `kept` between the numbers `from` and `to`, `to` nil for
  no end: {:times :value}, the value the last one, or nil where there is
  none, or ::not-kept where one may not be kept. Nil for `kept` nil."
  [kept [from to]]
  (cond
    (nil? kept) nil
    (may-miss? kept from to) ::not-kept
    :else
    (let [in (filterv #(and (<= from (number %)) (or (nil? to) (< (number %) to)))
                      (entries kept))]
      (when (seq in)
        {:times (count in) :value (nth (peek in) 1)}))))

(defn picker
  "A fn answering, of a site, what it evaluated to in the iteration
  `iteration` says, {:loop :index :enclosing :owners :starts}: the site of a
  name bound each time round of the loop, the index of the iteration, the
  sites of the loops it is in, of each site evaluated again each time round
  of one of these the innermost one's, and the :starts `ranges` takes. Of a
  call of which `kept-of` says what is kept: {:times :value}, or nil where
  the site was not evaluated then, or ::not-kept, or `default` where it is
  not evaluated again each time round.

  Nil where the iteration is not kept."
  [kept-of {:keys [owners] :as iteration}]
  (when-let [ranges (ranges kept-of iteration)]
    (fn [site default]
      (if-let [owner (get owners site)]
        (value-in (kept-of site) (get ranges owner))
        default))))

(defn kept-indices
  "The iterations kept of a loop whose name is bound at the site `loop`, of
  which `kept-of` says what is kept, as many values of a form as `kept`
  being kept first and as many last: their indices, from 1."
  [kept-of loop kept]
  (vec (for [[from to] (kept-iterations (:times (kept-of loop) 0) kept)
             i (range from (inc to))]
         i)))
