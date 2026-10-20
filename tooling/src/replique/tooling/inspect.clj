(ns replique.tooling.inspect
  "The :inspect ops (doc/server.md §6): a value, browsed from the editor a
  piece at a time, in a view. See replique.tooling.inspector for what a view
  is. Ported from replique's replique.inspect, whose protocol this speaks.

  A VIEW IS OPENED ON A SOURCE, which says where its value comes from:

    {:var \"ns/name\"}  what the var holds, on the JVM, or in Node, with
                      :dialect :cljs or :target :node
    {:debug {...}}    what was recorded: see replique.tooling.recorded/view

  and asked for again at each refresh. Nothing is watched: a Replique var
  has no watches, and there is no atom yet.

  A view belongs to the control connection that opened it, and goes with
  it: it holds the value it shows."
  (:require [replique.analyzer.env :as env]
            [replique.js.names :as names]
            [replique.tooling.inspector :as inspector]
            [replique.tooling.recorded :as recorded]
            [replique.tooling.recording :refer [refuse]])
  (:import [replique.lang Var]))

(defn views
  "What keeps the views of a server."
  []
  {:views (atom {}) :ids (atom 0)})

(defn- js-dialect? [{:keys [dialect target]}]
  (or (= :cljs dialect) (contains? #{:js :node} target)))

(defn- var-top
  "What a view of the var named `var` shows: what it holds, asked again at
  each refresh. In Node, `rt`, where there is one."
  [var rt]
  (let [sym (when (string? var) (symbol var))
        [ns n] (when (and sym (namespace sym)) [(namespace sym) (name sym)])]
    (when-not ns
      (refuse :unknown-var "A var is named in full, ns/name, got: " (pr-str var)))
    (if rt
      (let [root ["var" ns (names/munge n)]
            top #(let [[v] (inspector/node-values rt [[root []]])]
                   (if (identical? inspector/gone v)
                     (refuse :unknown-var "Node has no var " var)
                     v))]
        (top)
        top)
      (let [^Var v (or (Var/find ns n) (refuse :unknown-var "No var " var))]
        ;; an unbound var is shown as itself, holding nothing
        #(if (.isBound v) (.deref v) v)))))

(defn- top-of
  "What a view of `source` takes its value from, a function of nothing, and
  where the value is: [top target unit], `unit` the unit of a view of what
  was recorded."
  [r js-rt msg source]
  (cond
    (not (map? source))
    (refuse :invalid-message "The :inspect op needs the :source of the view, got: " (pr-str source))

    (:var source)
    (if (js-dialect? msg)
      [(var-top (:var source) (or (js-rt) (refuse :no-runtime "No JS runtime is running: open a JS repl")))
       :js nil]
      [(var-top (:var source) nil) :jvm nil])

    (:debug source)
    (let [{:keys [site loop calls cursor call arg iteration thread] :as debug} (:debug source)]
      (when thread
        (refuse :invalid-message "Replique has no debugger yet: no thread is stopped"))
      (when-not (and (map? debug)
                     (or (integer? site) (integer? loop) (integer? calls)
                         (and (map? cursor) (integer? (:unit cursor)) (integer? (:call cursor))
                              (integer? (:at cursor)) (or (nil? (:frame cursor)) (nat-int? (:frame cursor)))))
                     (or (nil? call) (integer? call))
                     (or (nil? arg) (nat-int? arg))
                     (or (nil? iteration) (and (map? iteration) (integer? (:site iteration))
                                               (integer? (:index iteration)))))
        (refuse :invalid-message "A view of what was recorded names a :site, and its :call, :arg"
                " and :iteration, {:site :index}; or a :loop, and its :call; or :calls, a unit;"
                " or a :cursor, {:unit :call :at :frame}, got: " (pr-str debug)))
      (let [u (recorded/view-unit r debug)]
        [#(recorded/view r debug) (if (:rt u) :js :jvm) u]))

    (:results source)
    (refuse :invalid-message "Replique keeps no *1 yet: there is no view of a repl's results")

    (:taps source)
    (refuse :invalid-message "Replique has no tap> yet: there is no view of what was tapped")

    :else
    (refuse :invalid-message "A view is opened on a :var or on what was recorded, :debug, got: "
            (pr-str source))))

(defn- opts-of [msg]
  (select-keys msg [:offset :limit :width :column :expand]))

(defn- the-view [{:keys [views]} msg]
  (or (get @views (:view msg))
      (refuse :unknown-view "No view " (pr-str (:view msg)) ": it was closed, or this server did not open it")))

(defn- node-of [msg]
  (let [node (:node msg)]
    (if (nat-int? node)
      node
      (refuse :invalid-message "A node is the number the view gave it, got: " (pr-str node)))))

(defn inspect
  "What `:inspect` answers: a view opened for the connection `conn` on the
  `:source` of `msg`, its id, the root's line and the first page of its
  children where it has some. `js-rt` answers the Node of the JS target,
  where there is one."
  [{:keys [views ids]} r js-rt conn msg]
  (let [[top target unit] (top-of r js-rt msg (:source msg))
        session (inspector/session top)
        id (swap! ids inc)]
    (swap! views assoc id {:id id :conn conn :session session :target target :unit unit})
    (assoc (inspector/shown session (opts-of msg)) :view id)))

(defn refresh
  "What `:inspect-refresh` answers: the view `:view` of the value now."
  [vs msg]
  (let [{:keys [session]} (the-view vs msg)]
    (inspector/refresh! session)
    (inspector/shown session (opts-of msg))))

(defn children
  "What `:inspect-children` answers: a page of the children of `:node`."
  [vs msg]
  (inspector/children (:model (:session (the-view vs msg))) (node-of msg) (opts-of msg)))

(def ^:private printed-up-to
  "How much of a value printed whole is sent, in chars."
  1000000)

(defn printed
  "What `:inspect-print` answers: `:node` printed whole."
  [vs msg]
  (inspector/printed (:model (:session (the-view vs msg))) (node-of msg) printed-up-to))

(defn path
  "What `:inspect-path` answers, which it can't yet: there is no core to
  write a path through a value with."
  [vs msg]
  (the-view vs msg)
  (node-of msg)
  (refuse :invalid-message "There is no code to a value yet: Replique has no core to write it with"))

(defn def!
  "What `:inspect-def` answers: the var `:name` of the namespace `:ns`, by
  default the namespace of the code recorded, for a view of what was
  recorded, defined to what `:node` holds, on the JVM target, whose
  compiler environment is `cenv`."
  [vs cenv msg]
  (let [{:keys [session target unit]} (the-view vs msg)
        node (node-of msg)
        n (:name msg)
        ns (or (:ns msg) (:ns unit))]
    (when (= :js target)
      (refuse :invalid-message "A value of Node is not defined from here"))
    (when-not (and (string? n) (simple-symbol? (symbol n)))
      (refuse :invalid-message "A var is defined by its :name, unqualified, got: " (pr-str n)))
    (when-not (and (string? ns) (seq ns))
      (refuse :unknown-ns "No namespace to define " n " in: name one, :ns"))
    (let [v (inspector/value (:model session) node)]
      (when (identical? inspector/gone v)
        (refuse :node-gone "What was there is no longer there"))
      (when (inspector/outline? v)
        (refuse :invalid-message "That is what was recorded said of values, and not a value"))
      (env/define-var! cenv (symbol ns n) {:defined? true})
      (.bindRoot (Var/intern ns n) v)
      {:var (str ns "/" n)})))

(defn close!
  "What `:inspect-close` answers: the view `:view` let go of."
  [{:keys [views]} msg]
  (swap! views dissoc (:view msg))
  {:closed true})

(defn closed!
  "Let go of the views of the connection `conn`, which closed."
  [{:keys [views]} conn]
  (swap! views (fn [vs] (into {} (remove #(= conn (:conn (val %)))) vs))))
