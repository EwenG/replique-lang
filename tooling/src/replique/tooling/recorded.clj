(ns replique.tooling.recorded
  "What a client asks of what was recorded (doc/server.md §5): what a call
  recorded, `:debug-recorded`, in one iteration of a loop of it too; where a
  cursor going through it goes, `:debug-cursor`; the calls recorded, as a
  tree, `:debug-calls`; and the first iteration where code is true,
  `:debug-find-iteration`. Ported from replique's replique.debug, whose
  protocol this speaks.

  All of it is answered on the numbers of the values a call kept, which
  replique.tooling.recording says of a call kept on the JVM and asks of Node
  for one kept there, and replique.tooling.iterations reads: the same code
  for both targets. Only the values a client is shown are printed, where
  they are kept.

  GOING THROUGH WHAT WAS RECORDED. Where a client is at is a cursor: a call,
  and the number of one of its values, which says the rest: the form that
  had that value, the iteration of each loop around it, and the calls it is
  in. A call started while another one was running was made by it, once it
  had as many values as its :parent says. The calls the cursor is in, and
  the loops around where it is in each, are its frames, innermost first,
  each at a form: in the innermost call the cursor's, and in a call that
  made the next one in, the first form that had a value after it made it.
  A move is made from one of the frames: to the value after or before, into
  a call made there, out of the call, to the same form in another iteration
  of a loop or in another call of the same unit. Where the frame is not the
  innermost, the cursor goes on into the call of the same unit made there,
  at the same form, as far down as there is one.

  WHAT A VIEW OF THE INSPECTOR SHOWS of what was recorded is an outline:
  what a value is said with, the iteration it is of, the call, the local,
  is not part of it. It is planned on the numbers too, and the values it
  shows are then fetched at once, where they are kept: on the JVM the
  values themselves, from Node mirrors of them (replique.tooling.inspector)."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [replique.analyzer :as a]
            [replique.js.dev :as js]
            [replique.js.emit :as js-emit]
            [replique.jvm.dev :as jvm]
            [replique.tooling.inspector :as inspector]
            [replique.tooling.iterations :as iterations]
            [replique.tooling.json :as json]
            [replique.tooling.recording :as recording :refer [refuse]])
  (:import [replique.lang IFn Keyword]))

(def ^:private values-kept
  "How many values of a form are kept in a call, first and last: what
  replique.tooling.recording keeps."
  10)

(def ^:private span-keys [:line :column :end-line :end-column])

(defn- unit-of [s n]
  (or (get-in s [:units n]) (refuse :unknown-unit "Unit " n " does not record")))

;; --- the code of a unit ----------------------------------------------------

(defn- text-at
  "The code the unit `u` wrote at `span`, or nil."
  [{:keys [text line column]} {l :line c :column el :end-line ec :end-column}]
  (when (and text line column l c el ec)
    (let [lines (str/split text #"\n" -1)
          offset (fn [ln col]
                   (let [row (- ln line)]
                     (when (< -1 row (count lines))
                       (+ (reduce + (map #(inc (count %)) (take row lines)))
                          (- col (if (zero? row) column 1))))))
          from (offset l c)
          to (offset el ec)]
      (when (and from to (<= 0 from to (count text)))
        (subs text from to)))))

(defn- code-text
  "The code `s` on one line, as long as a label is."
  [s]
  (let [s (str/replace (str/trim s) #"\s+" " ")]
    (if (> (count s) 70) (str (subs s 0 69) "…") s)))

(defn- site-label
  "What the value of the site `k` is shown under: the code written there,
  the params, of a fn's entry, or what a site made up is called."
  [s k]
  (let [{:keys [kind params unit line label] :as site} (get-in s [:sites k])]
    (cond
      label label
      (= :enter kind) (str "[" (str/join " " params) "]")
      :else (or (some-> (text-at (get-in s [:units unit]) site) code-text)
                (str "line " line)))))

(defn- place
  "Where the site `k` is written, as a frame says it, and what it is."
  [s k]
  (let [site (get-in s [:sites k])]
    (cond-> (select-keys site span-keys)
      (:kind site) (assoc :site-kind (name (:kind site))))))

(defn- in-loop?
  "Whether the site `k` of the unit `u` is evaluated again each time round
  of the loop `l`."
  [u l k]
  (boolean (some #{(:site l)} (get-in u [:in k]))))

(defn- iterating
  "What says which values of a call of the unit `u` are of the iteration
  `index` of the loop whose name is bound at the site `site`, as
  replique.tooling.iterations/picker takes it."
  [u {:keys [site index]}]
  (let [loops (:loops u)
        _ (or (some #(when (= site (:site %)) %) loops)
              (refuse :unknown-iteration "Site " site " starts no iteration of a loop"))
        around (set (get-in u [:in site]))
        chain (filterv #(around (:site %)) loops)
        chain-sites (set (map :site chain))]
    {:loop site
     :index index
     :enclosing (into [] (comp (map :site) (remove #{site})) chain)
     :starts (into {} (map (juxt :site :sites)) chain)
     :owners (into {} (for [[k in] (:in u)
                            :let [owner (last (filter chain-sites in))]
                            :when owner]
                        [k owner]))}))

;; --- what a call recorded --------------------------------------------------

(defn- value-said
  "What the site `n` recorded, evaluated `times` times, the last time to what
  printed as `value`, as `:debug-recorded` says it; `args`, an entry's,
  printed."
  [site n times value args]
  (let [said (merge {:site n :times times :value value} (select-keys site span-keys))]
    (case (:kind site)
      :enter (assoc said :kind "enter" :params (:params site []) :args (vec args))
      :local (assoc said :kind "local")
      said)))

(defn- loops-said
  "The loops of the unit `u` that went round in a call, of which `kept-of`
  says what is kept."
  [u kept-of]
  (vec (for [{:keys [site sites span fn]} (:loops u)
             :let [times (iterations/times-round kept-of sites)]
             :when times]
         (merge {:site site :iterations times :kept (iterations/kept-iterations times values-kept)}
                (when fn {:fn true})
                span))))

(defn recorded
  "What the unit `:unit` recorded in its call `:call`, the last one without,
  as `:debug-recorded` answers it: in its iteration `:iteration`, {:site
  :index}, of a loop, where there is one."
  [{:keys [state] :as r} {:keys [unit call iteration]}]
  (when-not (integer? unit)
    (refuse :invalid-message "A unit is named by its number, got: " (pr-str unit)))
  (when-not (or (nil? call) (integer? call))
    (refuse :invalid-message "A call is named by its number, got: " (pr-str call)))
  (when-not (or (nil? iteration)
                (and (map? iteration) (integer? (:site iteration)) (integer? (:index iteration))))
    (refuse :invalid-message "An iteration is {:site :index}, got: " (pr-str iteration)))
  (let [s @state
        u (unit-of s unit)
        it (when iteration (iterating u {:site (long (:site iteration)) :index (long (:index iteration))}))
        d (recording/call-data r unit (some-> call long))
        _ (when (and call (not= unit (:unit d)))
            (refuse :unknown-call "Call " call " is not kept any more"))
        kept-of (:kept-of d {})
        pick (if it
               (or (iterations/picker kept-of it)
                   (refuse :unknown-iteration "Iteration " (:index it) " is not kept"))
               (fn [_ default] default))
        picked (vec (for [site (sort (keys kept-of))
                          :when (not (contains? (:plain u) site))
                          :let [{:keys [times tail]} (kept-of site)
                                p (pick site {:times times :value (first (peek tail))})]
                          :when p]
                      [site p]))
        printed (recording/printed-values r unit (:call d)
                                          (for [[site p] picked :when (map? p)] [site (:value p)]))
        values (loop [[[site p] & more] picked, printed printed, acc []]
                 (cond
                   (nil? site) acc
                   (= ::iterations/not-kept p)
                   (recur more printed (conj acc (merge {:site site :not-kept true}
                                                        (select-keys (get-in s [:sites site]) span-keys))))
                   :else
                   (let [[value args :as said] (first printed)]
                     (recur more (rest printed)
                            (cond-> acc
                              said (conj (value-said (get-in s [:sites site]) site (:times p) value args)))))))]
    (cond-> {:calls (mapv #(select-keys % [:call :thread :at]) (:calls d))
             :loops (loops-said u kept-of)
             :values values}
      d (assoc :call (:call d))
      it (assoc :iteration {:site (:loop it) :index (:index it)}))))

;; --- going through it ------------------------------------------------------

(defn- calls-of
  "How the calls of the runtime the unit `unit` records in are known:
  {:call-of :latest}, a fn answering recording/call-data of a call, each
  asked once, and one answering it of the last call of `unit`."
  [r unit]
  (let [asked (atom {})]
    {:call-of (fn [c]
                (if (contains? @asked c)
                  (get @asked c)
                  (let [d (recording/call-data r unit c)] (swap! asked assoc c d) d)))
     :latest #(recording/call-data r unit nil)}))

(defn- site-at [d n] (some (fn [[k site]] (when (= k n) site)) (:events d)))
(defn- event-after [d n] (first (filter #(> (first %) n) (:events d))))
(defn- event-before [d n] (last (filter #(< (first %) n) (:events d))))
(defn- event-upto [d n] (last (filter #(<= (first %) n) (:events d))))

(defn- site-events
  "The numbers of the values of the site `k` kept of the call `d`, from
  `from` on and before `to`, nil for no end."
  [d k from to]
  (for [[n site] (:events d)
        :when (and (= k site) (<= from n) (or (nil? to) (< n to)))]
    n))

(defn- same-place
  "Of the values of the call `d` numbered `from` on and before `to`, the one
  of the site `k` that is the `ordinal`th of them, from 1, or the last, or
  nil where `k` has none there."
  [d k from to ordinal]
  (let [ns (site-events d k from to)]
    (when (seq ns) (nth ns (dec (min (max ordinal 1) (count ns)))))))

(defn- call-index
  "Which of the calls kept of its unit the call `d` is, from 0, or -1."
  [d]
  (.indexOf ^java.util.List (mapv :call (:calls d)) (:call d)))

(defn- levels
  "The calls the cursor at the value numbered `at` of the call `d` is in,
  innermost first: each {:data :at}, the call and the value it is at, and
  last, where the call that made the outermost one kept is not kept any
  more, {:gone c}."
  [call-of d at]
  (loop [d d, at at, acc []]
    (let [acc (conj acc {:data d :at at})
          [parent pos] (:parent d)
          made (or (:until d) pos)
          p (when (and parent (< (count acc) 1000)) (call-of parent))
          e (when p (or (event-after p made) (event-upto p made)))]
      (cond
        e (recur p (first e) acc)
        parent (conj acc {:gone parent})
        :else acc))))

(defn- loops-around
  "The loops of the code of the call `d` around its value numbered `at`
  that went round by then, innermost first: each of the unit's :loops, with
  the :index of the iteration `at` is of, nil where that is not kept, and
  how many times it went round in the call, :of."
  [s d at]
  (let [u (get-in s [:units (:unit d)])
        k (site-at d at)
        kept-of (:kept-of d)]
    (->> (:loops u)
         (filter #(in-loop? u % k))
         reverse
         (keep (fn [l]
                 (when-let [of (iterations/times-round kept-of (:sites l))]
                   (let [index (iterations/index-at kept-of (:sites l) at)]
                     (when-not (= 0 index)
                       (assoc l :index index :of of)))))))))

(defn- frames-of
  "The frames of the cursor in the calls `levels`, innermost first: in each
  call, the loops around where it is, then the call itself."
  [s levels]
  (vec
   (mapcat
    (fn [j {:keys [data at gone]}]
      (if gone
        [{:kind "gone" :level j :call gone}]
        (let [unit (:unit data)
              u (get-in s [:units unit])
              loops (loops-around s data at)
              index (call-index data)
              k (site-at data at)
              form {:form k :code (site-label s k)}]
          (concat
           (for [l loops]
             (merge {:kind "loop" :level j :unit unit :call (:call data) :loop (:site l)
                     :label (:label l) :of (:of l)}
                    form
                    (when (:fn l) {:fn true})
                    (when (:index l) {:index (:index l)})
                    (place s k)))
           [(merge {:kind "call" :level j :unit unit :call (:call data)
                    :label (:label u "evaluation") :of (count (:calls data)) :started (:at data)}
                   form
                   (when (:thread data) {:thread (:thread data)})
                   (when-not (neg? index) {:index (inc index)})
                   (when-let [l (first (filter :index loops))]
                     {:iteration {:site (:site l) :index (:index l)}})
                   (place s k))]))))
    (range) levels)))

(defn- made-until
  "The number of the last value the call that made the call `made`, made
  once it had `pos` values, had before it was over: the next one is the one
  it was made for."
  [made pos]
  (or (:until made) pos))

(defn- descend
  "Where the cursor goes, the value numbered `at` of the call `d` taking the
  place of the one the calls `below` were made for, innermost first, each
  {:data :at}: in each, the call of the same unit made there, at the same
  form, as many values of it in as it was, as far down as there is one.
  [call at note]."
  [s call-of d at below]
  (loop [d d, at at, [old & more] (reverse below)]
    (if-not old
      [d at nil]
      (let [unit (:unit (:data old))
            k (site-at (:data old) (:at old))
            ordinal (count (site-events (:data old) k 0 (inc (:at old))))
            from (or (first (event-before d at)) 0)
            child (->> (:children d)
                       (filter (fn [[pos _]] (< pos at)))
                       (keep (fn [[pos c]] (some-> (call-of c) (vector pos))))
                       (filter (fn [[made pos]] (<= from (made-until made pos) (dec at))))
                       (map first)
                       (filter #(and (= unit (:unit %)) (seq (:events %))))
                       last)]
        (cond
          (nil? child) [d at (str (:label (get-in s [:units unit]) "evaluation") " was not called there")]
          (same-place child k 0 nil ordinal) (recur child (same-place child k 0 nil ordinal) more)
          :else [child (ffirst (:events child)) nil])))))

(defn- unit-called
  "The unit recording the function the form at the value numbered `at` of
  the call `d` calls, by the name it is written with, or nil."
  [s d at]
  (let [{:keys [ns target] :as u} (get-in s [:units (:unit d)])
        head (some->> (text-at u (get-in s [:sites (site-at d at)]))
                      (re-find #"^\(\s*([^\s()\[\]{}\"]+)")
                      second)
        called (when head
                 (if (str/includes? head "/") (symbol head) (symbol ns head)))]
    (when called
      (some (fn [[n u]]
              (when (and (= target (:target u)) (some #{called} (:vars u)))
                n))
            (:units s)))))

(defn- into-call
  "Where a move into a call made at the value numbered `at` of the call `d`
  goes: the first value of the call it was made for, over since the value
  before, or of the call made after it, before the value after."
  [s call-of d at]
  (let [before (or (first (event-before d at)) 0)
        after (first (event-after d at))
        made (keep (fn [[pos c]]
                     (when-let [made (call-of c)]
                       (when (seq (:events made)) [pos made])))
                   (:children d))]
    (if-let [[_ made] (or (last (filter (fn [[pos made]] (<= before (made-until made pos) (dec at)))
                                        made))
                          (first (filter (fn [[pos]] (and (<= at pos) (or (nil? after) (< pos after))))
                                         made)))]
      [made (ffirst (:events made)) nil]
      (if-let [unit (unit-called s d at)]
        (refuse :no-move (:label (get-in s [:units unit])) " was called here, but that call is not"
                " recorded: it was not being recorded then, or it is not kept any more")
        (refuse :no-move "No call recorded was made here")))))

(defn- iteration-move
  "Where a move to another iteration of a loop goes, from the frame `f` of
  the cursor of `frames`, whose calls are `levels`: the same form in the
  next one kept, `move` \"next-iteration\", the one before, or the
  iteration `index`, \"iteration\". Of the loop of `f`, or of the innermost
  one of its call."
  [s call-of levels frames f move index]
  (let [level (:level f)
        l (if (= "loop" (:kind f))
            f
            (or (some #(when (and (= level (:level %)) (= "loop" (:kind %))) %) frames)
                (refuse :no-move "No loop went round around here")))
        {d :data at :at} (get levels level)
        u (get-in s [:units (:unit d)])
        kept-of (:kept-of d)
        current (:index l)
        _ (when (and (nil? current) (not= "iteration" move))
            (refuse :no-move "Which iteration this is is not kept"))
        kept (iterations/kept-indices kept-of (:loop l) values-kept)
        target (case move
                 "next-iteration" (first (filter #(> % current) kept))
                 "previous-iteration" (last (filter #(< % current) kept))
                 "iteration" (some #{index} kept))
        _ (when-not target
            (refuse :no-move (case move
                               "next-iteration" "This is the last iteration kept"
                               "previous-iteration" "This is the first iteration kept"
                               (str "Iteration " index " is not kept"))))
        range-of (fn [i] (get (iterations/ranges kept-of (iterating u {:site (:loop l) :index i}))
                              (:loop l)))
        [from to] (or (range-of target) (refuse :no-move "Iteration " target " is not kept"))
        k (site-at d at)
        ordinal (if-let [[from-now] (when current (range-of current))]
                  (count (site-events d k from-now (inc at)))
                  1)
        skipped (when (and current (not= "iteration" move) (> (abs (- target current)) 1))
                  (str "Iterations " (inc (min target current)) " to " (dec (max target current))
                       " are not kept"))]
    (if-let [n (same-place d k from to ordinal)]
      (let [[made at note] (descend s call-of d n (subvec levels 0 level))]
        [made at (some->> (remove nil? [skipped note]) seq (str/join " - ")) (:call d)])
      [d (or (first (event-after d (dec from))) at)
       (str/join " - " (remove nil? [skipped (str (site-label s k) " was not evaluated in"
                                                  " iteration " target)]))
       (:call d)])))

(defn- call-move
  "Where a move to another call of the same unit goes, from the frame `f` of
  the cursor whose calls are `levels`: the same form in the call after,
  `move` \"next-call\", the one before, or the call `index` among the ones
  kept, from 1, \"call\"."
  [s call-of levels f move index]
  (let [level (:level f)
        {d :data at :at} (get levels level)
        calls (mapv :call (:calls d))
        i (call-index d)
        c (cond
            (neg? i) (refuse :no-move "This call is not kept any more")
            (= "next-call" move) (get calls (inc i))
            (= "call" move) (when (and index (pos? index)) (get calls (dec index)))
            :else (when (pos? i) (get calls (dec i))))
        _ (when-not c
            (refuse :no-move (case move
                               "next-call" "This is the last call kept"
                               "previous-call" "This is the first call kept"
                               (str "Call " index " is not kept"))))
        made (or (call-of c) (refuse :no-move "That call is not kept any more"))
        k (site-at d at)]
    (when (empty? (:events made))
      (refuse :no-move "That call recorded nothing kept"))
    (if-let [n (same-place made k 0 nil (count (site-events d k 0 (inc at))))]
      (conj (descend s call-of made n (subvec levels 0 level)) c)
      [made (ffirst (:events made)) (str (site-label s k) " was not evaluated in that call") c])))

(defn- not-kept-between
  "What says how many values of the call `d` between the numbers `from` and
  `to` were not kept, or nil."
  [d from to]
  (let [kept (count (filter #(< from (first %) to) (:events d)))
        gone (- to from 1 kept)]
    (when (pos? gone) (str gone " values in between are not kept"))))

(def ^:private cursor-moves
  #{"next" "previous" "in" "out" "next-iteration" "previous-iteration" "iteration"
    "next-call" "previous-call" "call"})

(defn- move-cursor
  "Where the move `move` takes the cursor whose calls are `levels` and
  frames `frames`, from the frame numbered `frame`, `index` the iteration
  of a move \"iteration\", the call of a move \"call\": [call at note moved],
  `moved` the call the frame moved from is in after a move to another
  iteration or call. Refused where it goes nowhere."
  [s call-of levels frames frame move index]
  (let [f (get frames frame)
        {d :data at :at up :gone} (get levels (:level f))
        outer (get levels (inc (:level f)))]
    (when up (refuse :no-move "That call is not kept any more"))
    (case move
      "next" (if-let [[n] (first (filter #(> (first %) at) (:events d)))]
               [d n (not-kept-between d at n)]
               (if (:data outer)
                 [(:data outer) (:at outer) nil]
                 (refuse :no-move "This is the last value kept of the outermost call")))
      "previous" (if-let [[n] (last (filter #(< (first %) at) (:events d)))]
                   [d n (not-kept-between d n at)]
                   (if-let [[n] (when (:data outer) (event-upto (:data outer) (second (:parent d))))]
                     [(:data outer) n nil]
                     (refuse :no-move "This is the first value kept of the outermost call")))
      "out" (if (:data outer)
              [(:data outer) (:at outer) nil]
              (refuse :no-move "This is the outermost call kept"))
      "in" (into-call s call-of d at)
      ("next-iteration" "previous-iteration" "iteration")
      (iteration-move s call-of levels frames f move index)
      ("next-call" "previous-call" "call") (call-move s call-of levels f move index))))

(defn cursor
  "Where the move `:move` takes the cursor at the value `:at` of the call
  `:call` of the unit `:unit`, or at the value of the site `:site` nearest
  it, before it where there is one, or its last, from its frame `:frame`,
  the innermost without, as `:debug-cursor` answers it. Without `:call`, a
  call of the unit is the last one, and so is a call not kept any more,
  which is said, and not moved from."
  [{:keys [state] :as r} {:keys [unit call at site frame move index] :as msg}]
  (when-not (and (integer? unit) (or (nil? call) (integer? call)) (or (nil? at) (integer? at))
                 (or (nil? site) (integer? site)) (or (nil? frame) (nat-int? frame))
                 (or (nil? move) (contains? cursor-moves move))
                 (or (nil? index) (integer? index)))
    (refuse :invalid-message "A cursor is at the value :at of the :call of a :unit, or at"
            " its :site, and moves by :move, one of " (str/join ", " (sort cursor-moves))
            ", from its :frame, got: "
            (pr-str (select-keys msg [:unit :call :at :site :frame :move :index]))))
  (let [s (do (unit-of @state unit) @state)
        {:keys [call-of latest]} (calls-of r unit)
        [d note] (if-let [d (when call (call-of call))]
                   [d nil]
                   [(or (latest) (refuse :unknown-call "Unit " unit " was not called yet"))
                    (when call "That call is not kept any more - this is the last one")])
        _ (when (empty? (:events d))
            (refuse :unknown-call "That call recorded nothing kept"))
        at (if-let [ns (when site (seq (site-events d site 0 nil)))]
             (or (when (and at (not note)) (or (last (filter #(<= % at) ns)) (first ns)))
                 (last ns))
             (or (when (and at (not note)) (first (event-upto d at)))
                 (ffirst (:events d))))
        ls (levels call-of d at)
        fs (frames-of s ls)
        frame (min (or frame 0) (dec (count fs)))
        f (get fs frame)
        [d at said moved] (if (and move (not note))
                            (move-cursor s call-of ls fs frame move index)
                            [d at nil nil])
        fs (frames-of s (levels call-of d at))
        ;; the frame moved from stays the one looked at, where it is still
        ;; there, or its call
        in-moved (fn [kind loop]
                   (first (keep-indexed (fn [i g]
                                          (when (and (= moved (:call g)) (= kind (:kind g))
                                                     (= loop (:loop g)))
                                            i))
                                        fs)))
        frame (if moved
                (or (in-moved (:kind f) (:loop f)) (in-moved "call" nil) 0)
                0)]
    (cond-> {:unit (:unit d) :call (:call d) :at at :frame frame :frames fs}
      (or note said) (assoc :note (or note said)))))

;; --- the calls, as a tree --------------------------------------------------

(defn- call-summary
  "A call as a client is shown it, from what its runtime told, see
  recording/calls-told: its :label, which of the calls kept of its unit it
  is, :index, of :of, what it was called with, :args, and returned,
  :value, where they are kept, how many calls it made that are kept,
  :made, and, among calls made in none kept, `root`, whether it was made in
  one not kept any more, :made-in-gone; of a call recorded by its calls
  only that threw, :threw, its :value what it threw."
  [s root {:keys [call unit at thread parent made index of] lasts :last}]
  (let [u (get-in s [:units unit])
        latest (fn [ks] (->> (keep #(some-> (get lasts %) (assoc :site %)) ks)
                             (sort-by :number)
                             last))
        args (latest (:entries u))
        value (latest (:returns u))]
    (cond-> {:unit unit :call call :label (:label u "evaluation") :started at :made (or made 0)}
      thread (assoc :thread thread)
      (pos? (or index 0)) (assoc :index index :of of)
      args (assoc :args (:args args))
      value (assoc :value (:value value))
      (and (:calls-only u) (= (:site value) (second (:returns u)))) (assoc :threw true)
      (and root parent) (assoc :made-in-gone true))))

(defn- sites-of
  "The entries and the returns of the units `us`."
  [us]
  (vec (distinct (mapcat #(concat (:entries %) (:returns %)) us))))

(defn calls
  "The calls kept made in none kept, of every unit that records, on the JVM
  and in Node, oldest first, or with `:unit` and `:call`, the calls that
  call made, as `:debug-calls` answers them."
  [{:keys [state] :as r} {:keys [unit call] :as msg}]
  (when-not (or (and (nil? unit) (nil? call)) (and (integer? unit) (integer? call)))
    (refuse :invalid-message "The calls a call made are asked by its :unit and its :call, got: "
            (pr-str (select-keys msg [:unit :call]))))
  (let [s @state
        units (vals (:units s))
        told (fn [rt parent]
               (recording/calls-told r rt parent (sites-of (filter #(identical? rt (:rt %)) units))))]
    {:calls
     (if unit
       (let [{:keys [rt]} (unit-of s unit)]
         (when (and rt (js/closed? rt))
           (refuse :unknown-unit "Unit " unit " was recorded in a Node that is gone"))
         (mapv #(call-summary s false %) (told rt call)))
       (->> (cons nil (distinct (keep :rt units)))
            (remove #(and % (js/closed? %)))
            (mapcat #(told % nil))
            (map #(call-summary s true %))
            (sort-by (juxt :started :call))
            vec))}))

;; --- views of it, as the inspector shows them -------------------------------

(def ^:private not-kept (inspector/note "not-kept"))
(def ^:private not-recorded (inspector/note "not-recorded"))

(defn- last-number
  "The number of the last value of the site `k` kept of a call, of which
  `kept-of` says what is kept, or nil."
  [kept-of k]
  (some-> (kept-of k) :tail peek first))

(defn- realize
  "The value `plan` says, the values it names fetched where the unit `u`
  keeps them, all at once. A plan is {:outline [[label plan where] ...]},
  {:value [call site number arg]}, the value numbered so of a site of a
  call, of an entry's with `arg`, the arg-th of what its fn was called
  with, or what it shows itself."
  [r u plan]
  (let [refs (vec (distinct (keep :value (tree-seq #(and (map? %) (contains? % :outline)) #(map second (:outline %))
                                                   plan))))
        fetched (if-let [rt (:rt u)]
                  (inspector/node-values rt (for [[c site n arg] refs]
                                              [["call" c site n] (if arg [arg] [])]))
                  (vec (mapcat (fn [same-call] (recording/values-kept-of r (ffirst same-call) (map rest same-call)))
                               (partition-by first refs))))
        by-ref (zipmap refs fetched)
        real (fn real [x]
               (cond
                 (not (map? x)) x
                 (:outline x) (inspector/outline (map (fn [[label p where]] [label (real p) where]) (:outline x)))
                 (:value x) (let [v (get by-ref (:value x))]
                              (if (or (= ::recording/none v) (identical? inspector/gone v)) not-kept v))
                 :else x))]
    (real plan)))

(defn- param-columns
  "The params of the fn whose entry is the site `entry`, each [name i], the
  ith of what it was called with."
  [s entry]
  (vec (map-indexed (fn [i p] [p i]) (remove #{"&"} (get-in s [:sites entry :params])))))

(defn- site-plan
  "What the site `site` evaluated to in the call `call`, the last value, of
  an entry's with `arg` the arg-th of what its fn was called with; in its
  iteration `iteration`, {:site :index}, of a loop of the call, the last
  one without; and without either, in each call kept, by call."
  [r s u {:keys [site call arg iteration]}]
  (let [unit (get-in s [:sites site :unit])
        it (when iteration (iterating u iteration))
        one (fn [d]
              (let [kept-of (:kept-of d {})
                    default (some->> (last-number kept-of site) (hash-map :value))
                    p (if it
                        ((or (iterations/picker kept-of it)
                             (refuse :unknown-iteration "Iteration " (:index it) " is not kept"))
                         site default)
                        default)]
                (cond
                  (= ::iterations/not-kept p) not-kept
                  (map? p) {:value [(:call d) site (:value p) arg]}
                  :else not-recorded)))]
    (if (or call it)
      (if-let [d (recording/call-data r unit call)] (one d) not-recorded)
      (let [rows (keep-indexed (fn [i {c :call}]
                                 (let [d (recording/call-data r unit c)]
                                   (when (and d (last-number (:kept-of d {}) site))
                                     [(str "call " (inc i)) (one d)])))
                               (:calls (recording/call-data r unit nil)))]
        (if (seq rows) {:outline rows} not-recorded)))))

(defn- calls-plan
  "What each call kept of the unit `unit` was called with, by call, each
  param by its name, and what the site `site`, where it is not an entry,
  evaluated to last in it, under the code written there."
  [r s u {unit :calls site :site}]
  (let [form (when (and site (not= :enter (get-in s [:sites site :kind]))) site)]
    {:outline
     (vec (keep-indexed
           (fn [i {c :call}]
             (when-let [d (recording/call-data r unit c)]
               (let [kept-of (:kept-of d {})
                     entry (->> (:entries u) (filter #(last-number kept-of %)) (sort-by #(last-number kept-of %)) last)]
                 [(str "call " (inc i))
                  {:outline (concat (when entry
                                      (for [[name j] (param-columns s entry)]
                                        [name {:value [c entry (last-number kept-of entry) j]}]))
                                    (when-let [n (when form (last-number kept-of form))]
                                      [[(site-label s form) {:value [c form n nil]}]]))}])))
           (:calls (recording/call-data r unit nil))))}))

(defn- loop-plan
  "What each iteration kept of the loop whose name is bound at the site
  `loop` did in the call `call`, the last one without, by iteration: the
  names it binds each time round, the params of a fn, and the ones bound
  in it, each to what it was bound to then. With `site`, a form evaluated
  again each time round, what it evaluated to last in each too, under the
  code written there; with `only` as well, that alone, or the name, where
  `site` is one, of the iterations it was evaluated in."
  [r s u {:keys [loop call site only]}]
  (let [l (or (some #(when (= loop (:site %)) %) (:loops u))
              (refuse :unknown-iteration "Site " loop " starts no iteration of a loop"))
        d (recording/call-data r (get-in s [:sites loop :unit]) call)
        kept-of (:kept-of d {})
        bound (set (:sites l))
        names (concat (if (:fn l)
                        (map (fn [[name i]] [name loop i]) (param-columns s loop))
                        (map (fn [name k] [name k nil]) (:names l) (:sites l)))
                      (->> (:in u)
                           (filter (fn [[k in]] (and (= loop (peek in)) (not (bound k))
                                                     (= :local (get-in s [:sites k :kind])))))
                           (map key)
                           (sort-by #(mapv (get-in s [:sites %]) [:line :column]))
                           (map (fn [k] [(get-in s [:sites k :name]) k nil]))))
        named (when (and site (not (:fn l))) (some #(when (= site (second %)) %) names))
        code (when (and site (not named) (in-loop? u l site)) [(site-label s site) site nil])
        alone (or named code)
        columns (cond (and only alone) [alone] code (concat names [code]) :else names)
        it (cond-> (iterating u {:site loop :index 1})
             ;; what it evaluated to last in each iteration of this loop,
             ;; one inside it going round in it or not
             code (assoc-in [:owners site] loop))
        rows (vec (for [index (iterations/kept-indices kept-of loop values-kept)
                        :let [pick (iterations/picker kept-of (assoc it :index index))]
                        :when pick]
                    [(str "iteration " index)
                     {:outline (vec (keep (fn [[name k i]]
                                            (let [v (pick k nil)]
                                              (when (map? v) [name {:value [(:call d) k (:value v) i]}])))
                                          columns))}]))]
    (if (and only alone)
      {:outline (vec (keep (fn [[label row]] (when-let [[_ v] (first (:outline row))] [label v])) rows))}
      {:outline rows})))

(defn- innermost
  "Of `locals`, as `:scope` says them, outermost first, the innermost of
  each name."
  [locals]
  (->> (reverse locals)
       (reduce (fn [[seen acc] [name :as l]] (if (seen name) [seen acc] [(conj seen name) (conj acc l)]))
               [#{} []])
       second
       reverse
       vec))

(defn- cursor-plan
  "What the cursor `cursor`, {:unit :call :at :frame}, is at, in its frame
  `frame`, the innermost without: the value numbered :at of the call, on
  the line under the code written there, and \"locals\", each local the
  source wrote in scope there, the innermost of a name, to the last value
  it was bound to by then, :not-kept where that may not be kept."
  [r s {:keys [unit call at frame]}]
  (let [{:keys [call-of]} (calls-of r unit)
        d (or (call-of call) (refuse :unknown-call "Call " call " is not kept any more"))
        ls (levels call-of d (or (first (event-upto d at)) (ffirst (:events d))))
        fs (frames-of s ls)
        {d :data at :at} (get ls (:level (get fs (or frame 0) (first fs))))
        _ (when-not d (refuse :unknown-call "That call is not kept any more"))
        k (site-at d at)
        u (get-in s [:units (:unit d)])
        kept-of (:kept-of d)
        locals (->> (innermost (get-in u [:scope k]))
                    (keep (fn [[name ls arg]]
                            (let [l (iterations/latest (kept-of ls) at)]
                              (cond
                                (= ::iterations/not-kept l) [name not-kept]
                                l [name {:value [(:call d) ls (first l) arg]}])))))]
    [u {:outline (cond-> [[(site-label s k) {:value [(:call d) k at nil]} :above]]
                   (seq locals) (conj ["locals" {:outline (vec locals)}]))}]))

(defn view-unit
  "The unit a view of what was recorded, of the :debug source `source`, is
  of."
  [{:keys [state]} {:keys [site loop calls cursor]}]
  (let [s @state]
    (unit-of s (cond cursor (:unit cursor)
                     calls calls
                     :else (or (get-in s [:sites (or site loop) :unit])
                               (refuse :unknown-unit "No site " (or site loop) " records"))))))

(defn view
  "What a view of what was recorded, of the :debug source `source`, shows:
  what a site evaluated to, {:site :call :arg :iteration}; what each call
  of a unit was called with, {:calls :site}; what each iteration of a loop
  did, {:loop :call :site :only}; what a cursor is at, {:cursor {:unit
  :call :at :frame}}. See doc/server.md §6."
  [{:keys [state] :as r} {:keys [site loop calls cursor iteration] :as source}]
  (let [s @state
        u (view-unit r source)]
    (when (and (:rt u) (js/closed? (:rt u)))
      (refuse :unknown-unit "That was recorded in a Node that is gone"))
    (cond
      cursor (let [[u plan] (cursor-plan r s cursor)] (realize r u plan))
      calls (realize r u (calls-plan r s u source))
      loop (realize r u (loop-plan r s u source))
      :else (realize r u (site-plan r s u (cond-> source
                                            iteration (update :iteration #(select-keys % [:site :index]))))))))

;; --- code evaluated against an iteration -------------------------------------

;; What `:debug-find-iteration` answers: the first iteration of a loop of a
;; call where code is true, the code a fn of the locals the source wrote in
;; scope at a place of the unit's code, each bound to what it was in the
;; iteration. A local the loop binds or binds in it is what it was bound to
;; last in the iteration; one further out, last in the call. One whose value
;; is not known, a form not evaluated then, or a value not kept, is bound to
;; :replique/not-recorded. The code is compiled once, in the unit's
;; namespace, on its target, and run where the values are kept: on the JVM
;; here, in Node there.

(def ^:private unknown-local (Keyword/intern "replique" "not-recorded"))

(defn- scope-of
  "The locals the source wrote in scope at the site `k` of the unit `u`, as
  `:scope` says them; at a name bound, that one too, and at a name a loop
  binds each time round, all of them, bound together."
  [u k site]
  (let [scope (get-in u [:scope k] [])]
    (if (= :local (:kind site))
      (if-let [l (some #(when (some #{k} (:sites %)) %) (:loops u))]
        (into scope (map (fn [name b] [name b nil]) (:names l) (:sites l)))
        (conj scope [(:name site) k nil]))
      scope)))

(defn- scope-at
  "The locals the source wrote in scope at `line` and `column` of the code
  of the unit `unit`: those of the innermost form written there, or around
  it, as `scope-of` says them."
  [s unit line column]
  (let [pos [line column]
        start (fn [[_ site]] [(:line site) (:column site)])
        around (filter (fn [[_ site :as e]]
                         (and (= unit (:unit site)) (:line site) (:end-line site)
                              (<= (compare (start e) pos) 0)
                              (< (compare pos [(:end-line site) (:end-column site)]) 0)))
                       (:sites s))]
    (if (seq around)
      (let [inner (reduce #(if (pos? (compare (start %2) (start %1))) %2 %1) around)]
        (->> around
             (filter #(= (start %) (start inner)))
             (map (fn [[k site]] (scope-of (get-in s [:units unit]) k site)))
             (apply max-key count)))
      [])))

(defn- searched
  "Of the iterations `indices`, the ones to look at, in that order: after
  the iteration `from`, or before it, `backward`; all of them without."
  [indices from backward]
  (cond
    (nil? from) (if backward (reverse indices) indices)
    backward (reverse (filter #(< % from) indices))
    :else (filter #(> % from) indices)))

(defn- compiled
  "The code `code`, the body of a fn of the locals `names`, analysed in the
  namespace `ns` of the target `target`: on the JVM the fn, in JS a
  function expression of the runtime answering it. Refused where it
  doesn't read or compile."
  [{:keys [id rt cenv]} ns names code]
  (let [ns (symbol ns)
        text (str "(fn* ([" (str/join " " names) "]\n" code "\n))")
        {:keys [units diagnostics]} (try (a/analyze-source cenv text {:ns ns :target id})
                                         (catch clojure.lang.ExceptionInfo e
                                           (refuse :compilation "The code doesn't compile: " (ex-message e))))
        error (first (filter #(= :error (:severity %)) (concat diagnostics (mapcat :diagnostics units))))
        _ (when error (refuse :compilation "The code doesn't compile: " (:message error)))
        _ (when-not (= 1 (count units)) (refuse :compilation "The code is one form, or none"))
        r (first units)]
    (try
      (if (= :jvm id)
        (let [{:keys [value error]} (jvm/run-record rt ns r)]
          (when error (refuse :compilation "The code doesn't compile: " (:message error)))
          value)
        (js-emit/form-js ns (:node r)))
      (catch clojure.lang.ExceptionInfo e
        (if (recording/refused e)
          (throw e)
          (refuse :compilation "The code doesn't compile: " (ex-message e)))))))

(defn- jvm-first-where
  "Of `rows`, [index refs]s, the first where the fn `f` is true, called
  with the values of the call `c` the refs say, [call site number arg] or
  nil: {:index}, nil where there is none, with the message of the first
  thing it threw, :error, where it threw."
  [r c ^IFn f rows]
  (let [refs (vec (distinct (keep identity (mapcat second rows))))
        by-ref (zipmap refs (recording/values-kept-of r c (map rest refs)))
        arg (fn [ref]
              (let [v (if ref (get by-ref ref) ::recording/none)]
                (if (identical? ::recording/none v) unknown-local v)))]
    (loop [[[index refs] & more] rows, error nil]
      (if (nil? index)
        (cond-> {:index nil} error (assoc :error error))
        (let [[found thrown] (try (let [v (.applyTo f (object-array (map arg refs)))]
                                    [(not (or (nil? v) (identical? Boolean/FALSE v))) nil])
                                  (catch Throwable t
                                    [false (or (ex-message t) (.getName (class t)))]))]
          (if found
            {:index index}
            (recur more (or error thrown))))))))

(defn- node-first-where
  "What jvm-first-where answers, the fn of the function expression `f`,
  the values kept in the Node of `rt`."
  [rt f rows]
  (let [ref-js (fn [ref]
                 (if-let [[c site n arg] ref]
                   (recording/js-array [(js-emit/string-literal "call") c site n (if arg arg "null")])
                   "null"))
        rows-js (recording/js-array (for [[index refs] rows]
                                      (recording/js-array [index (recording/js-array (map ref-js refs))])))
        {:keys [value thrown]} (js/run-js rt (str "(function ($rt) {\n"
                                                  "return globalThis.$repliqueRecorder.firstWhere((" f ")($rt), "
                                                  rows-js ");\n})")
                                          "replique/tooling/find-iteration")]
    (when thrown
      (refuse :runtime (:message thrown)))
    (some-> value edn/read-string json/read-str)))

(defn find-iteration
  "What `:debug-find-iteration` answers: of the loop whose name is bound at
  `:site`, in the call `:call` of the unit `:unit`, the last one without,
  the first iteration kept after `:from`, or the last before it,
  `:backward`, all of them without, where `:code` is true, evaluated with
  the locals in scope at `:line` and `:column` bound to what they were
  then: {:index}, nil where there is none, with :error, the message of
  what the code threw, where it threw. `target-of` answers the target of
  an id, :jvm or :js, which compiles the code."
  [{:keys [state] :as r} target-of {:keys [unit call site from backward line column code] :as msg}]
  (when-not (and (integer? unit) (integer? site) (or (nil? call) (integer? call))
                 (or (nil? from) (integer? from)) (pos-int? line) (pos-int? column) (string? code))
    (refuse :invalid-message "An iteration is found in a :unit, a :call or the last, of the loop"
            " whose name is bound at :site, after the iteration :from, by the :code evaluated at"
            " :line and :column, got: "
            (pr-str (select-keys msg [:unit :call :site :from :line :column :code]))))
  (let [s @state
        u (unit-of s unit)
        _ (when (and (:rt u) (js/closed? (:rt u)))
            (refuse :unknown-unit "Unit " unit " was recorded in a Node that is gone"))
        d (recording/call-data r unit (some-> call long))
        _ (when (or (nil? d) (not= unit (:unit d)))
            (refuse :unknown-call (if call
                                    (str "Call " call " is not kept any more")
                                    (str "Unit " unit " was not called yet"))))
        site (long site)
        it (iterating u {:site site :index 1})
        kept-of (:kept-of d {})
        locals (innermost (scope-at s unit line column))
        rows (vec (for [index (searched (iterations/kept-indices kept-of site values-kept) from backward)
                        :let [pick (iterations/picker kept-of (assoc it :index index))]
                        :when pick]
                    [index (mapv (fn [[_ k arg]]
                                   (let [v (pick k (some->> (last-number kept-of k) (hash-map :value)))]
                                     (when (map? v) [(:call d) k (:value v) arg])))
                                 locals)]))
        target (target-of (:target u))
        _ (when (and (:rt u) (not (identical? (:rt u) (:rt target))))
            (refuse :unknown-unit "Unit " unit " was recorded in a Node that is gone"))
        f (compiled target (:ns u) (map first locals) code)
        {:keys [index error]} (if (:rt u)
                                (node-first-where (:rt u) f rows)
                                (jvm-first-where r (:call d) f rows))]
    (cond-> {:index index} error (assoc :error error))))
