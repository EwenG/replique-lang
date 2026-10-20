(ns replique.tooling.recording
  "What records (doc/recording.md §6, doc/server.md §5): the calls and the
  values that code compiled to be recorded reports, kept to be asked about
  by `:debug-recorded`. Ported from replique's replique.debug and
  replique/debug.cljs, whose protocol this speaks.

  On the JVM they are kept here. In JS they are kept in Node, where they
  live, by recording.js, the same model in JS, and asked for there; what
  each unit and each site is, known at compile time, is kept here for both.

  A form evaluated with `#replique/src {:record true}` is a unit. It is
  compiled to report to a hook of its own (replique.lang.Hook), and its
  sites are numbered here, with what each is. A definition stays a unit
  until it is let go of, or until a var it defined is defined again; any
  other form, until it is let go of.

  WHICH CALL A VALUE IS IN IS KNOWN FROM THE CODE: the call is what `enter`
  answered, handed down lexically. A call starts on entering a fn of a
  definition made in no call; a fn made in a call records in that call,
  even once it is over. A value in no call is the unit's last call's, or a
  first one's, its evaluation. Which call made a call is the one running
  on the thread when it started.

  Of a unit, the last `calls-kept` calls made in no other call kept are
  kept. A call made in another is kept as long as that one is, and as long
  as the value it was made for is: the first one that call had once it was
  over, kept of it or not as any value is. Of the calls made for the same
  value, in a loop that records nothing else, the first `values-kept` and
  the last ones. Of all the calls, `calls-kept-in-all` at most. Of a form in
  a call, the first `values-kept` values and the last ones, each numbered in
  the call, and how many times it was evaluated. The values are kept as
  they are, and printed when asked for.

  What a client asks of it is answered by replique.tooling.recorded, on the
  numbers of the values a call kept, the same on both targets: this
  namespace says what is kept of a call, `call-data`, and prints the values
  asked for, `printed-values`, or hands them to the inspector,
  `values-kept-of`.

  A FUNCTION WHOSE CODE IS NOT RECORDED, a library's, is recorded by its
  calls only, `record-calls!`: its var is made to hold a function that
  records each call and calls the one it held. Nothing is compiled again;
  defined again, it is wrapped again. Only its calls made while a call
  recorded runs on the thread are recorded, each made in that one: anywhere
  else it is only called. A unit of its own, :calls-only, whose sites are
  made up, said by their :label: its entry, what it returned and what it
  threw. It is let go of once its code is recorded, and with the last unit
  of code recorded in its runtime.

  Clients are told a unit is recorded with `debug-instrumented`, that it is
  not any more with `debug-uninstrumented`, and that it recorded with
  `debug-recorded`, at most every `tell-every` ms."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [replique.analyzer.env :as env]
            [replique.analyzer.sites :as sites]
            [replique.ir :as ir]
            [replique.js.dev :as js]
            [replique.js.emit :as js-emit]
            [replique.js.names :as names]
            [replique.jvm.dev :as jvm]
            [replique.tooling.iterations :as iterations]
            [replique.tooling.json :as json])
  (:import [java.util ArrayDeque]
           [replique.lang AFn Hook IFn Printer Var]))

(def ^:private calls-kept
  "How many calls of a unit made in no other call kept are kept, the last
  ones."
  10)

(def ^:private calls-kept-in-all
  "How many calls are kept at most, of all units: the last ones, and the
  ones running."
  10000)

(def ^:private values-kept
  "How many values of a form are kept in a call, the first ones, and as many
  of the last ones."
  10)

(def ^:private tell-every
  "How often clients are told that a unit recorded, at most, in ms."
  200)

(defn recorder
  "What keeps what is recorded on the JVM. `tell!` sends an event frame to
  every client.

    :state    {:units {n unit} :sites {n site} :calls (sorted-map n call)}
              and the last numbers given, :unit-n :site-n :call-n
    :running  per thread, the numbers of the calls entered and not exited
              yet, the innermost last
    :telling  the units clients are about to be told recorded"
  [tell!]
  {:state (atom {:units {} :sites {} :calls (sorted-map) :unit-n 0 :site-n 0 :call-n 0})
   :running (ThreadLocal/withInitial (reify java.util.function.Supplier (get [_] (ArrayDeque.))))
   :telling (atom #{})
   :tell! tell!})

(defn refuse
  "Refuse what a client asked, an error of the kind `kind`."
  [kind & message]
  (throw (ex-info (apply str message) {::refused kind})))

(defn refused
  "The kind of the refusal `e` is, an error a client is answered with, or
  nil."
  [e]
  (::refused (ex-data e)))

;; --- calls -----------------------------------------------------------------

;; A unit: {:target :ns :file :line :column :end-line :end-column :text
;; :definition :vars :label :calls :plain :shown}, and what `structure`
;; says. The code its form is, :text, starting at :line and :column; what it
;; is called, :label; its calls kept oldest first; the sites of its locals
;; whose value is their init's; and :shown once clients were told. A unit in
;; JS has the Node it runs in, :rt, and its calls are kept there. One
;; recording the calls of a var only has :calls-only, and on the JVM
;; :wrapping, {:var :original :wrapper}, what the var held and holds.
;;
;; A call: {:unit :thread :at :count :values :open :parent :children
;; :until}. What the thread named :thread did from :at, in ms: :count values,
;; and of each site {:times :values :numbers :head}, how many times it was
;; evaluated, its last values, numbered :numbers in the call, and its first
;; ones, [number value]s. :open until it is exited. Made in a call, once that
;; one had :count values, its :parent [call count], and of that one, one of
;; its :children [count call]; once it is over and that one had :until
;; values, made for the next one.

(defn- update-some
  "`m` with the value at `path` updated by `f`, where there is one."
  [m path f & args]
  (if (some? (get-in m path)) (apply update-in m path f args) m))

(defn- forget-calls
  "`state` without the calls `cs`, and the calls they made, and so on."
  [state cs]
  (loop [state state todo (vec cs)]
    (if-let [c (peek todo)]
      (let [todo (pop todo)
            {:keys [unit parent children] :as call} (get-in state [:calls c])]
        (if-not call
          (recur state todo)
          (recur (-> state
                     (update :calls dissoc c)
                     (update-some [:units unit :calls] (fn [cs] (filterv #(not= c %) cs)))
                     (update-some [:calls (first parent) :children]
                                  (fn [ch] (filterv #(not= c (second %)) ch))))
                 (into todo (map second) children))))
      state)))

(defn- new-call
  "[state c]: `state` with a new call `c` of the unit `unit`, made in the
  call `parent` where it is still kept."
  [state unit parent]
  (let [c (inc (:call-n state))
        p (when parent (get-in state [:calls parent]))
        state (-> state
                  (assoc :call-n c)
                  (assoc-in [:calls c] {:unit unit :thread (.getName (Thread/currentThread))
                                        :at (System/currentTimeMillis) :count 0 :values {}
                                        :open true :parent (when p [parent (:count p)])
                                        :children []})
                  (update-some [:units unit :calls] conj c)
                  (cond-> p (update-in [:calls parent :children] conj [(:count p) c])))
        kept (:calls state)
        made-in-none (filterv #(not (contains? kept (first (:parent (get kept %)))))
                              (get-in state [:units unit :calls]))
        state (forget-calls state (drop-last calls-kept made-in-none))
        over (- (count (:calls state)) calls-kept-in-all)]
    [(cond-> state
       (pos? over) (forget-calls (take over (remove #(:open (get-in state [:calls %]))
                                                    (keys (:calls state))))))
     c]))

(defn- latest
  "[state c]: the last call of the unit `unit`, or a first one, its
  evaluation."
  [state unit parent]
  (if-let [c (peek (get-in state [:units unit :calls]))]
    [state c]
    (new-call state unit parent)))

(defn- dropping
  "`v` without its first element, a vector of its own: a subvec would keep
  what it drops."
  [v]
  (into [] (rest v)))

(defn- made-for
  "Of the calls the call `call` of `state` made, the ones made for its value
  numbered `i`: the next one it had once they were over."
  [state call i]
  (filterv #(= (dec i) (get-in state [:calls % :until])) (map second (:children call))))

(defn- keep-value
  "`state` with `v` kept as what the site `site` evaluated to in the call `c`,
  where it is still kept, and without the calls made for the value it keeps
  no more."
  [state c site v]
  (if-let [call (get-in state [:calls c])]
    (let [i (inc (:count call))
          {:keys [times values numbers head]
           :or {times 0 values [] numbers [] head []}} (get-in call [:values site])
          values (conj values v)
          numbers (conj numbers i)
          over? (> (count values) values-kept)
          out (first numbers)]
      (cond-> (assoc-in state [:calls c]
                        (-> call
                            (assoc :count i)
                            (assoc-in [:values site]
                                      {:times (inc times)
                                       :values (cond-> values over? dropping)
                                       :numbers (cond-> numbers over? dropping)
                                       :head (cond-> head (< (count head) values-kept) (conj [i v]))})))
        (and over? (not-any? #(= out (first %)) head)) (forget-calls (made-for state call out))))
    state))

(defn- ended
  "`state` with the call `c` over: made for the next value of the call that
  made it. Of the ones made for the same value, the first `values-kept` and
  the last ones are kept."
  [state c]
  (let [call (get-in state [:calls c])
        p (get-in state [:calls (first (:parent call))])]
    (if (and p (nil? (:until call)))
      (let [state (assoc-in state [:calls c :until] (:count p))
            same (made-for state p (inc (:count p)))]
        (forget-calls state (take (- (count same) (* 2 values-kept)) (drop values-kept same))))
      state)))

(defn- changed!
  "Tell clients that the unit `unit` recorded, once every `tell-every` ms at
  most: a fn called in a loop mustn't cost the editor a message a call."
  [{:keys [state telling tell!]} unit]
  (let [[before _] (swap-vals! telling conj unit)]
    (when-not (contains? before unit)
      (doto (Thread. ^Runnable
                     (fn []
                       (Thread/sleep (long tell-every))
                       (swap! telling disj unit)
                       (let [{:keys [calls shown]} (get-in @state [:units unit])]
                         (when (and shown (seq calls))
                           (tell! {:tag "event" :event "debug-recorded" :unit unit
                                   :call (peek calls) :calls (count calls)}))))
                     "replique-recorded")
        (.setDaemon true)
        (.start)))))

;; --- the hook --------------------------------------------------------------

(deftype Call [call own])

(defn- jvm-hook
  "What the code of the unit `unit`, on the JVM, reports to. A call is a Call: the
  number of the call kept it records in, nil once the unit is gone, and
  whether it is its own, started on entering."
  ^Hook [{:keys [state ^ThreadLocal running] :as r} unit definition]
  (let [live? #(contains? (:units @state) unit)
        running-now #(.peekLast ^ArrayDeque (.get running))
        latest! #(let [c (volatile! nil)]
                   (swap! state (fn [s] (let [[s n] (latest s unit (running-now))] (vreset! c n) s)))
                   @c)]
    (reify Hook
      (enter [_ site outer args]
        (let [^Call call (cond
                           (some? outer) (Call. (.-call ^Call outer) false)
                           (not (live?)) (Call. nil false)
                           definition (let [c (volatile! nil)]
                                        (swap! state (fn [s] (let [[s n] (new-call s unit (running-now))]
                                                               (vreset! c n)
                                                               s)))
                                        (.addLast ^ArrayDeque (.get running) @c)
                                        (changed! r unit)
                                        (Call. @c true))
                           :else (Call. (latest!) false))]
          ;; what it was called with is what its entry evaluated to
          (when (and (.-call call) (pos? (alength ^objects args)) (live?))
            (swap! state keep-value (.-call call) site (aclone ^objects args)))
          call))
      (value [_ site v call]
        (when (live?)
          (when-let [c (if (some? call) (.-call ^Call call) (latest!))]
            (swap! state keep-value c site v)
            (changed! r unit)))
        v)
      (exit [_ call]
        (let [^Call call call]
          (when (.-own call)
            (.removeLastOccurrence ^ArrayDeque (.get running) (.-call call))
            (swap! state #(-> %
                              (update-some [:calls (.-call call)] assoc :open false)
                              (ended (.-call call))))
            (when (live?) (changed! r unit))))))))

;; --- in Node ---------------------------------------------------------------

(defn install!
  "Make the Node of the runtime `rt` record, which it does with
  recording.js."
  [rt]
  (let [{:keys [thrown]} (js/run-js rt (slurp (io/resource "replique/tooling/recording.js"))
                                    "replique/tooling/recording.js")]
    (when thrown
      (throw (ex-info (str "Node couldn't record: " (:message thrown)) {})))))

(defn in-node
  "What `$repliqueRecorder.<method>(args...)` answers in the Node of `rt`,
  as it prints. Each arg is JS."
  [rt method & args]
  (let [{:keys [value thrown]}
        (js/run-js rt (str "(function ($rt) { return globalThis.$repliqueRecorder." method
                           "(" (str/join ", " args) "); })")
                   "replique/tooling/recording")]
    (when thrown
      (throw (ex-info (str "Node's recording failed: " (:message thrown)) {})))
    value))

(defn told!
  "What Node's recorder told, `text`: that a unit recorded."
  [{:keys [state tell!]} text]
  (let [{:keys [unit call calls]} (edn/read-string text)]
    (when (:shown (get-in @state [:units unit]))
      (tell! {:tag "event" :event "debug-recorded" :unit unit :call call :calls calls}))))

(defn js-array [xs] (str "[" (str/join ", " xs) "]"))

;; --- units -----------------------------------------------------------------

(defn- described
  "The unit `n` as clients are told about it."
  [n unit]
  (cond-> (merge {:unit n :ns (:ns unit) :record true}
                 (select-keys unit [:file :line :column :end-line :end-column]))
    (:calls-only unit) (assoc :calls-only true)))

(defn- same-runtime?
  "Whether the units `u` and `v` record in the same runtime."
  [u v]
  (and (= (:target u) (:target v)) (identical? (:rt u) (:rt v))))

(defn uninstrument!
  "Let go of the unit `n` and of what it recorded, saying so to the clients
  that were told of it. Whether there was one. The var of a unit recording
  its calls only holds what it held again, where it holds what it was made
  to; the last unit of code recorded in a runtime takes the ones recording
  calls only there with it."
  [{:keys [state tell!] :as r} n]
  (let [[before after] (swap-vals! state
                                   (fn [s]
                                     (if-let [unit (get-in s [:units n])]
                                       (-> s
                                           (forget-calls (:calls unit))
                                           (update :units dissoc n)
                                           (update :sites #(into {} (remove (fn [[_ site]] (= n (:unit site)))) %)))
                                       s)))]
    (when-let [unit (get-in before [:units n])]
      (when-let [{:keys [^Var var original wrapper]} (:wrapping unit)]
        (when (identical? wrapper (.deref var))
          (.bindRoot var original)))
      (when-let [rt (:rt unit)]
        (when-not (js/closed? rt)
          (try (in-node rt "uninstrument" n) (catch Exception _ nil))))
      (when (:shown unit)
        (tell! {:tag "event" :event "debug-uninstrumented" :unit n}))
      (when-not (:calls-only unit)
        (let [left (filter #(same-runtime? unit (val %)) (:units after))]
          (when (every? (comp :calls-only val) left)
            (doseq [[k _] left] (uninstrument! r k)))))
      true)))

(declare rewrap!)

(defn defined!
  "The vars `vars` were defined again on the target `target`, :jvm or :js,
  by another unit than `except`: the units that defined them are let go of,
  the code they hold not being theirs any more. A var whose calls only are
  recorded is wrapped again, where it is not recorded now."
  [{:keys [state] :as r} target vars except]
  (when (seq vars)
    (let [vars (set vars)]
      (doseq [[n unit] (:units @state)
              :when (and (not= n except) (= target (:target unit)) (some vars (:vars unit)))]
        (if (and (:calls-only unit) (nil? except))
          (rewrap! r n unit)
          (uninstrument! r n))))))

(defn runtime-gone!
  "The Node of `rt` went away, and what it recorded with it."
  [{:keys [state] :as r} rt]
  (doseq [[n unit] (:units @state) :when (identical? rt (:rt unit))]
    (uninstrument! r n)))

(defn- plain-locals
  "The sites of the locals of `node` a let binds to a call or a literal: a
  local whose value is its init's, shown there."
  [node]
  (into #{} (keep (fn [n]
                    (when (and (= :binding (:op n)) (:site n) (= :let (:local n)))
                      (let [init (:init n)]
                        (when (or (:site init)
                                  (and (= :const (:op init)) (not (coll? (:form init)))))
                          (:site n))))))
        (ir/nodes node)))

(def ^:private span-keys [:line :column :end-line :end-column])

(defn- return-site
  "The site of the form whose value the fn method body `b` returns, or
  nil."
  [b]
  (cond (:site b) (:site b)
        (= :do (:op b)) (recur (:ret b))))

(defn- structure
  "What the code of a unit is, `record` marked, its sites being `sites`, to
  go through what it recorded:

    :loops    the loops that go round in a call of it, outermost first, each
              {:site :sites :names :span :label}: the site of the first name
              it binds each time round, of all of them, the names, and the
              loop's form. An arity of a fn is one too, with :fn, that goes
              round once for each call of it: its :site is its entry. Not
              one of a definition in no other fn, which is what is called,
              nor one that calls itself, nor one of no param
    :in       of each site, the sites of the loops it is evaluated again
              each time round of, outermost first
    :scope    of each site, the locals the source wrote in scope there,
              outermost first: each [name site arg], the site recording
              what it is bound to, and of a param, `arg`, which of what its
              fn was called with it is, its entry being the site
    :entries  the entries of its outermost fns
    :returns  the sites of the last form of each, whose value it returns"
  [{:keys [node expansions]} sites definition]
  (let [loops (volatile! [])
        in (volatile! {})
        scope (volatile! {})
        entries (volatile! [])
        returns (volatile! [])
        at! (fn [n chain env]
              (when-let [s (:site n)]
                (vswap! in assoc s chain)
                (vswap! scope assoc s env)))
        bound (fn [env b] (cond-> env (:site b) (conj [(str (:name b)) (:site b) nil])))
        top (fn [o] (last (take-while some? (iterate #(:parent (get expansions %)) o))))
        walk (fn walk [n chain env in-fn? self?]
               (case (:op n)
                 :let
                 (let [env (reduce (fn [env b]
                                     (some-> (:init b) (walk chain env in-fn? false))
                                     (at! b chain env)
                                     (bound env b))
                                   env (:bindings n))]
                   (at! n chain env)
                   (walk (:body n) chain env in-fn? false))

                 :loop
                 (let [bs (filterv :site (:bindings n))
                       site (:site (first bs))
                       inner (cond-> chain site (conj site))]
                   (at! n chain env)
                   (when site
                     (let [e (some->> (:origin n) top (get expansions))
                           names (mapv (comp str :name) bs)]
                       (vswap! loops conj {:site site :sites (mapv :site bs) :names names
                                           :span (select-keys (or (:call e) (:span n)) span-keys)
                                           :label (str/join " " (cons (if e (name (:macro e)) "loop*")
                                                                      names))})))
                   ;; an init is evaluated once, before the loop goes round
                   (let [env (reduce (fn [env b]
                                       (some-> (:init b) (walk chain env in-fn? false))
                                       (at! b inner env)
                                       (bound env b))
                                     env (:bindings n))]
                     (walk (:body n) inner env in-fn? false)))

                 :fn
                 (let [id (:id (:local n))
                       self? (boolean (and id (some #(and (= :local (:op %)) (= id (:id %)))
                                                    (mapcat ir/nodes (:methods n)))))]
                   (at! n chain env)
                   (doseq [m (:methods n)] (walk m chain env in-fn? self?)))

                 :fn-method
                 (let [e (:site n)
                       outermost (not in-fn?)
                       params (:params (get sites e))
                       loop? (and e (not (and definition outermost)) (seq (:params n)) (not self?))
                       inner (cond-> chain loop? (conj e))]
                   (when (and e outermost)
                     (vswap! entries conj e)
                     (some->> (return-site (:body n)) (vswap! returns conj)))
                   (when loop?
                     (vswap! loops conj {:site e :sites [e] :fn true :names (vec (remove #{"&"} params))
                                         :span (select-keys (:span n) span-keys)
                                         :label (str "fn [" (str/join " " params) "]")}))
                   (let [env (cond-> env
                               e (into (map-indexed (fn [i p] [(str (:name p)) e i]) (:params n))))]
                     (at! n inner env)
                     (doseq [c (ir/children n)] (walk c inner env true false))))

                 (do (at! n chain env)
                     (doseq [c (ir/children n)] (walk c chain env in-fn? false)))))]
    (when node (walk node [] [] false false))
    {:loops @loops :in @in :scope @scope :entries @entries :returns @returns}))

(defn- site!
  "Number the site `m`, of the unit `n`, as the pass describes it."
  [state n {:keys [kind span params name outermost label]}]
  (let [k (volatile! nil)]
    (swap! state (fn [s]
                   (let [i (inc (:site-n s))]
                     (vreset! k i)
                     (-> s
                         (assoc :site-n i)
                         (assoc-in [:sites i]
                                   (cond-> (merge {:unit n :kind kind}
                                                  (select-keys span [:line :column :end-line :end-column]))
                                     params (assoc :params (mapv str params))
                                     name (assoc :name (str name))
                                     label (assoc :label label)
                                     (some? outermost) (assoc :outermost outermost)))))))
    @k))

(defn- fn-value?
  "Whether the var named `q` holds a fn."
  [q]
  (let [^Var v (Var/find (namespace q) (name q))]
    (and v (.isBound v) (instance? IFn (.deref v)))))

(defn- new-unit!
  "The number of the new unit `unit`, kept from then on."
  [state unit]
  (let [k (volatile! nil)]
    (swap! state (fn [s]
                   (let [i (inc (:unit-n s))]
                     (vreset! k i)
                     (-> s (assoc :unit-n i) (assoc-in [:units i] unit)))))
    @k))

(defn- shown!
  "Tell the clients of the unit `n`, from then on shown."
  [{:keys [state tell!]} n]
  (swap! state update-some [:units n] assoc :shown true)
  (when-let [unit (get-in @state [:units n])]
    (tell! (merge {:tag "event" :event "debug-instrumented"} (described n unit)))))

(defn run-record
  "Run the record `record`, analysed in namespace `ns`, on the target
  `target`, compiled to be recorded as a new unit, `where` saying the place
  of its form: {:file :line :column :end-line :end-column}. As
  replique.jvm.dev/run-record answers, or replique.js.dev/run-record, which
  `file` and `on-out` are for. A unit that doesn't compile, or throws, is
  let go of."
  [{:keys [state] :as r} {:keys [id rt]} ns record where file on-out]
  (let [vars (:defines record)
        definition (boolean (seq vars))
        label (cond definition (name (first vars))
                    (:line where) (str "line " (:line where))
                    :else "evaluation")
        n (new-unit! state (cond-> (merge where {:target id :ns (str ns) :definition definition
                                                 :vars vars :label label :calls []})
                             (= :js id) (assoc :rt rt)))
        marked (try (sites/mark record {:site #(site! state n %) :locals? true})
                    (catch Throwable t (uninstrument! r n) (throw t)))
        _ (swap! state (fn [s] (update-some s [:units n] merge
                                             {:plain (plain-locals (:node marked))}
                                             (structure marked (:sites s) definition))))
        result (try (if (= :js id)
                      (js/run-record rt ns marked file on-out
                                     {:hook (str "globalThis.$repliqueRecorder.unit(" n ", " definition ")")})
                      (jvm/run-record rt ns marked {:hook (jvm-hook r n definition)}))
                    (catch Throwable t (uninstrument! r n) (throw t)))]
    (if (or (:error result) (:thrown result))
      (do (uninstrument! r n) result)
      (do
        ;; the evaluation of a fn's definition is not one of its calls,
        ;; which are what is looked at
        (when definition
          (if (= :js id)
            (in-node rt "evaluated" n
                     (js-array (map #(js-array [(js-emit/string-literal (namespace %))
                                                (js-emit/string-literal (names/munge (name %)))])
                                    vars)))
            (when (every? fn-value? vars)
              (swap! state (fn [s] (forget-calls s (get-in s [:units n :calls])))))))
        (defined! r id vars n)
        (shown! r n)
        result))))

;; --- calls only ------------------------------------------------------------

(defn- calls-only-unit!
  "A new unit recording the calls of the var `q` of the target `target`
  only, `info` what its compiler knows of it: [n sites], its sites [entry
  returned threw]. Its entry is shown under the params of a fn of one
  arity, and `args` otherwise."
  [{:keys [state]} {:keys [id rt]} q {:keys [span arglists]}]
  (let [n (new-unit! state (cond-> (merge (select-keys span [:file :line :column])
                                          {:target id :ns (namespace q) :definition true :vars [q]
                                           :label (str q) :calls-only true :calls [] :plain #{}
                                           :loops [] :in {} :scope {}})
                             (= :js id) (assoc :rt rt)))
        params (when (= 1 (count arglists)) (first arglists))
        entry (site! state n (cond-> {:kind :enter :outermost true}
                               params (assoc :params params)
                               (not params) (assoc :label "args")))
        returned (site! state n {:kind :value :label "returned"})
        threw (site! state n {:kind :value :label "threw"})]
    (swap! state update-some [:units n] assoc :entries [entry] :returns [returned threw])
    [n [entry returned threw]]))

(defn- calls-recorder
  "What the var of the unit `n`, recording its calls only, holds instead of
  `f`, which it calls: a call made while a call recorded runs on the thread
  is recorded at the unit's sites `sites`, any other only made."
  ^IFn [{:keys [^ThreadLocal running] :as r} n [entry returned threw] ^IFn f]
  (let [^Hook hook (jvm-hook r n true)
        entry (int entry)
        returned (int returned)
        threw (int threw)
        call (fn [^objects args]
               (if (nil? (.peekLast ^ArrayDeque (.get running)))
                 (.applyTo f args)
                 (let [c (.enter hook entry nil args)]
                   (try
                     (.value hook returned (.applyTo f args) c)
                     (catch Throwable t
                       (.value hook threw t c)
                       (throw t))
                     (finally (.exit hook c))))))]
    (proxy [AFn] []
      ;; past 20 args, the rest come in an array
      (invoke [& args]
        (call (if (= 21 (count args))
                (object-array (concat (butlast args) (last args)))
                (object-array args))))
      (applyTo [args] (call args)))))

(defn- wrap!
  "Make the var `v` of the unit `n`, recording its calls only, hold what
  records the calls of `f`."
  [{:keys [state] :as r} n ^Var v f]
  (let [unit (get-in @state [:units n])
        w (calls-recorder r n (into (:entries unit) (:returns unit)) f)]
    (swap! state update-some [:units n] assoc :wrapping {:var v :original f :wrapper w})
    (.bindRoot v w)))

(defn- rewrap!
  "The var of the unit `n`, recording its calls only, was defined again: it
  is wrapped again, or let go of where it holds no fn now."
  [r n {:keys [rt wrapping]}]
  (if rt
    (when-not (and (not (js/closed? rt)) (edn/read-string (in-node rt "rewrap" n)))
      (uninstrument! r n))
    (let [{:keys [^Var var wrapper]} wrapping
          f (when (.isBound var) (.deref var))]
      (cond
        (identical? f wrapper) nil
        (instance? IFn f) (wrap! r n var f)
        :else (uninstrument! r n)))))

(defn record-calls!
  "Record the calls only of the vars `qs` of the target `target`, of which
  its compiler knows: {q why} of the ones that can't be. One already is,
  and one whose code is recorded can't be."
  [{:keys [state] :as r} {:keys [id rt cenv] :as target} qs]
  (let [here (filter #(same-runtime? {:target id :rt (when (= :js id) rt)} (val %)) (:units @state))
        calls-only (set (for [[_ u] here :when (:calls-only u)] (first (:vars u))))
        code (set (for [[_ u] here :when (not (:calls-only u)) q (:vars u)] q))
        refused (into {} (for [q qs
                               :let [info (env/find-var cenv q)
                                     why (cond
                                           (not (:defined? info)) "no such var"
                                           (:macro? info) "a macro"
                                           (:dynamic? info) "dynamic: what is bound is not the var's"
                                           (contains? code q) "its code is recorded")]
                               :when why]
                           [q why]))
        made (vec (for [q (distinct qs)
                        :when (not (or (contains? calls-only q) (contains? refused q)))]
                    (into [q] (calls-only-unit! r target q (env/find-var cenv q)))))
        not-fn (if (= :js id)
                 (let [skipped (into {} (some-> (in-node rt "wrap"
                                                         (js-array (for [[q n sites] made]
                                                                     (js-array (concat [n (js-emit/string-literal (namespace q))
                                                                                        (js-emit/string-literal (names/munge (name q)))]
                                                                                       sites)))))
                                                edn/read-string json/read-str))]
                   (for [[q n] made :when (contains? skipped n)] [q n (get skipped n)]))
                 (for [[q n] made
                       :let [^Var v (Var/find (namespace q) (name q))
                             f (when (and v (.isBound v)) (.deref v))]
                       :when (or (not (instance? IFn f)) (do (wrap! r n v f) false))]
                   [q n "not a function"]))
        not-fn (vec not-fn)
        gone (set (map second not-fn))]
    (doseq [[_ n] made]
      (if (contains? gone n) (uninstrument! r n) (shown! r n)))
    (into refused (map (fn [[q _ why]] [q why])) not-fn)))

;; --- what is kept of a call ---------------------------------------------

(def ^:private printed-up-to
  "How much of a value said to a client is printed, in chars."
  1000)

(defn printed-briefly
  "`x` printed to be shown by the form it is the value of: its beginning,
  the rest being to browse."
  [x]
  (let [s (try (Printer/print x)
               (catch Throwable t (str "#<unprintable " (or (ex-message t) (.getName (class t))) ">")))]
    (if (> (count s) printed-up-to) (str (subs s 0 printed-up-to) "...") s)))

(defn- numbers-kept
  "What `kept-of` of replique.tooling.iterations answers of a site of which
  `times`, the numbers `head` and `tail` are kept: each value its number."
  [times head tail]
  {:times times :head (mapv #(vector % %) head) :tail (mapv #(vector % %) tail)})

(defn- data
  "`call-data` of what is said of a call: {:call :unit :thread :at :parent
  :children :until :calls}, and `sites`, of each site [times head tail]."
  [said sites]
  (let [kept (into {} (map (fn [[site times head tail]] [site (numbers-kept times head tail)])) sites)]
    (assoc said
           :kept-of kept
           :events (iterations/events kept (keys kept)))))

(defn- jvm-call-data [s c]
  (when-let [{:keys [unit thread at parent children until values]} (get-in s [:calls c])]
    (data {:call c :unit unit :thread thread :at at :parent parent :children children :until until
           :calls (mapv (fn [k] (let [{:keys [thread at]} (get-in s [:calls k])]
                                  {:call k :thread thread :at at}))
                        (get-in s [:units unit :calls]))}
          (for [[site {:keys [times numbers head]}] values]
            [site times (mapv first head) numbers]))))

(defn- node-call-data [answer]
  (when-let [{:keys [call unit at parent children until calls sites]}
             (some-> answer edn/read-string json/read-str)]
    (data {:call call :unit unit :at at :parent parent :children (vec children) :until until
           :calls (mapv (fn [[k at]] {:call k :at at}) calls)}
          sites)))

(defn call-data
  "What is kept of the call `c` of the runtime the unit `unit` records in,
  its last one with `c` nil, or nil where there is none:

    {:call :unit :thread :at :parent :children :until :calls :kept-of :events}

  :calls the calls kept of its unit, oldest first, each {:call :thread
  :at}; :kept-of, of each site, what replique.tooling.iterations takes, the
  numbers of its values standing for them; :events the values kept, [number
  site]s in order. A unit not recording is refused."
  [{:keys [state]} unit c]
  (let [s @state
        u (or (get-in s [:units unit]) (refuse :unknown-unit "Unit " unit " does not record"))]
    (if-let [rt (:rt u)]
      (do (when (js/closed? rt) (refuse :unknown-unit "Unit " unit " was recorded in a Node that is gone"))
          (node-call-data (if c (in-node rt "call" c) (in-node rt "last" unit))))
      (jvm-call-data s (or c (peek (:calls u)))))))

(defn printed-values
  "The values of the call `c` of the runtime of the unit `unit` numbered so,
  `pairs` of [site number], printed: each [value args], `args` an entry's,
  printed, or nil for one not kept."
  [{:keys [state]} unit c pairs]
  (let [s @state
        u (get-in s [:units unit])]
    (cond
      (empty? pairs) []
      (:rt u) (or (some-> (in-node (:rt u) "printed" c (js-array (map js-array pairs)))
                          edn/read-string json/read-str vec)
                  (vec (repeat (count pairs) nil)))
      :else
      (let [call (get-in s [:calls c])]
        (mapv (fn [[site i]]
                (let [{:keys [values numbers head]} (get-in call [:values site])
                      j (if numbers (.indexOf ^java.util.List numbers i) -1)
                      e (if (neg? j) (some #(when (= i (first %)) %) head) [i (nth values j)])]
                  (when e
                    (let [v (second e)]
                      [(printed-briefly v)
                       (when (= :enter (get-in s [:sites site :kind])) (mapv printed-briefly v))]))))
              pairs)))))

(defn values-kept-of
  "The values of the call `c`, kept on the JVM, `triples` [site number
  arg]: each the value itself, or of an entry's, with `arg`, the arg-th of
  what its fn was called with; ::none for one not kept."
  [{:keys [state]} c triples]
  (let [call (get-in @state [:calls c])]
    (mapv (fn [[site i arg]]
            (let [{:keys [values numbers head]} (get-in call [:values site])
                  j (if numbers (.indexOf ^java.util.List numbers i) -1)
                  e (if (neg? j) (some #(when (= i (first %)) %) head) [i (nth values j)])]
              (cond
                (nil? e) ::none
                arg (let [v (second e)] (if (and (some? v) (< -1 arg (count v))) (nth v arg) ::none))
                :else (second e))))
          triples)))

(defn- thrown
  "What a call recorded by its calls only threw, `t`, as a line says it."
  [^Throwable t]
  (str (.getName (class t)) (some->> (.getMessage t) (str ": "))))

(defn calls-told
  "The calls kept of the runtime `rt`, nil for the JVM, made in none kept,
  or with `parent`, the ones that call made, oldest first: each {:call :unit
  :at :thread :parent :made :index :of :last}, :index which of the calls
  kept of its unit it is, from 1, 0 for none, of :of, and :last, of each
  site of `sites` it has a value of, {site {:number :value :args}}, printed;
  what a call recorded by its calls only threw, by its class and message."
  [{:keys [state]} rt parent sites]
  (if rt
    (vec (for [{:keys [last] :as told} (some-> (in-node rt "calls" (if parent parent "null") (js-array sites))
                                               edn/read-string json/read-str)]
           (assoc told :last (into {} (map (fn [[site n value args]]
                                             [site (cond-> {:number n :value value} args (assoc :args args))]))
                                   last))))
    (let [s @state
          all (:calls s)
          told (fn [c {:keys [unit at thread parent children values]}]
                 (let [kept (get-in s [:units unit :calls] [])]
                   {:call c :unit unit :at at :thread thread :parent (first parent)
                    :made (count children) :index (inc (.indexOf ^java.util.List kept c)) :of (count kept)
                    :last (into {} (for [site sites
                                         :let [{vs :values :keys [numbers]} (get values site)]
                                         :when (seq vs)
                                         :let [v (peek vs)]]
                                     [site (cond-> {:number (peek numbers)
                                                    :value (if (and (instance? Throwable v)
                                                                    (= "threw" (get-in s [:sites site :label])))
                                                             (thrown v)
                                                             (printed-briefly v))}
                                             (= :enter (get-in s [:sites site :kind]))
                                             (assoc :args (mapv printed-briefly v)))]))}))]
      (if parent
        (vec (for [[_ c] (get-in all [parent :children]) :let [call (get all c)] :when call]
               (told c call)))
        (vec (for [[c call] all :when (not (contains? all (first (:parent call))))]
               (told c call)))))))

(defn uninstrument
  "What `:debug-uninstrument` answers: the unit `:unit` let go of, or every
  one, with the ones that went with them."
  [{:keys [state] :as r} {:keys [unit]}]
  (when-not (or (nil? unit) (integer? unit))
    (refuse :invalid-message "A unit is named by its number, got: " (pr-str unit)))
  (let [before (:units @state)]
    (doseq [n (if unit [(long unit)] (sort (keys before)))]
      (uninstrument! r n))
    ;; and the ones recording calls only that went with the last code
    {:uninstrumented (vec (sort (remove (set (keys (:units @state))) (keys before))))}))
