(ns replique.js.dev
  "Dev mode in JS (doc/compilation.md §2.2), in a long-lived Node process:
  each top-level form analysed for JS, compiled to a function of the
  runtime (replique.js.emit) and run there, which is the path of the REPL.

    (def rt (runtime))                 ; starts node, which dials back
    (def cenv (replique.analyzer/environment))
    (eval-form rt cenv '(defn f [] 1) {:ns 'app.core})
    ;; => {:record r :ns app.core :js \"(function ($rt) {...})\" :value \"nil\"}
    (load-source rt cenv \"(ns app.core) (defn f [] 1) (f)\" {:file \"app/core.rpq\"})
    (eval-form rt cenv '(f) {:ns 'app.core :record {:site f :hook \"globalThis.H\"}})
    (close rt)

  A form evaluated with :record is compiled to be recorded
  (doc/recording.md): `:site` chooses its sites, as
  replique.analyzer.sites/mark's, and :hook is a JS expression Node
  evaluates once, before the form runs, for the object it reports to.

  A value comes back as the text the kernel's printer wrote, and an
  exception as {:class :message :data}, data printed too: they live in
  Node, and the JVM only ever sees their text.

  The JVM listens and Node connects, as in replique-cljs's REPL, so there is
  no port to configure. runtime.js and the Node driver (node_repl.mjs) are
  copied into a directory of the runtime's own, removed on close. One
  request at a time: answers are matched to requests by order."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [replique.analyzer :as a]
            [replique.analyzer.host :as host]
            [replique.analyzer.sites :as sites]
            [replique.analyzer.unit :as u]
            [replique.js.emit :as emit])
  (:import [java.io BufferedReader Closeable File InputStreamReader OutputStreamWriter Writer]
           [java.net InetAddress ServerSocket Socket SocketTimeoutException]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.concurrent LinkedBlockingQueue TimeUnit]))

(defn- delete-tree! [^File f]
  (when (.isDirectory f)
    (run! delete-tree! (.listFiles f)))
  (.delete f))

(defn- copy-resource! [res ^File to]
  (with-open [in (io/input-stream (or (io/resource res)
                                      (throw (ex-info (str "no resource " res) {}))))]
    (io/copy in to)))

(defn- pump!
  "Copy what Node writes to `out`, on a daemon thread."
  [in ^Writer out]
  (doto (Thread. ^Runnable
         (fn []
           (with-open [r (io/reader in)]
             (loop []
               (let [n (.read r)]
                 (when-not (neg? n)
                   (.write out n)
                   (when (= n (int \newline)) (.flush out))
                   (recur)))))))
    (.setDaemon true)
    (.start)))

(defrecord NodeRuntime [^Process proc ^Socket sock ^LinkedBlockingQueue lines timeout ^Writer w ^File dir ^Writer out]
  Closeable
  (close [_]
    (.close sock)
    (.destroy proc)
    (delete-tree! dir)))

(defn- read-lines!
  "Read what Node sends, on a daemon thread: an event is given to `on-event`
  there and then, every other line queued on `lines` for the request it
  answers, and ::closed once Node is gone."
  [^BufferedReader in ^LinkedBlockingQueue lines on-event]
  (doto (Thread. ^Runnable
         (fn []
           (try
             (loop []
               (when-let [line (.readLine in)]
                 (if (str/starts-with? line "event\t")
                   (when on-event
                     (try (on-event (edn/read-string (subs line 6)))
                          (catch Throwable _ nil)))
                   (.put lines line))
                 (recur)))
             (catch java.io.IOException _ nil)
             (finally (.put lines ::closed))))
         "replique-node")
    (.setDaemon true)
    (.start)))

(defn runtime
  "Start Node and wait for it to dial back. Options:

    :out       where Node's own output goes, default *out*
    :timeout   ms to wait for Node to connect, and for each answer; default
               60000. A runtime that times out is closed: what it was
               running may never answer.
    :on-event  called with each string Node's code tells the JVM, on a
               thread of its own, which must not wait for Node"
  ([] (runtime {}))
  ([{:keys [out timeout on-event] :or {timeout 60000}}]
   (let [out (or out *out*)
         dir (.toFile (Files/createTempDirectory "replique-js-dev" (make-array FileAttribute 0)))
         _ (copy-resource! "replique/runtime.js" (io/file dir "runtime.js"))
         _ (copy-resource! "replique/js/node_repl.mjs" (io/file dir "node_repl.mjs"))
         ^ServerSocket server (doto (ServerSocket. 0 1 (InetAddress/getByName "127.0.0.1"))
                  (.setSoTimeout timeout))
         ^Process proc (try
                (-> (ProcessBuilder. ^"[Ljava.lang.String;" (into-array String ["node" "node_repl.mjs" (str (.getLocalPort server))]))
                    (.directory dir)
                    (.redirectErrorStream true)
                    (.start))
                (catch java.io.IOException e
                  (.close server)
                  (delete-tree! dir)
                  (throw (ex-info (str "Could not start node: " (ex-message e)
                                       ". JS dev mode needs node on the PATH.")
                                  {} e))))]
     (pump! (.getInputStream proc) out)
     (let [^Socket sock (try (.accept server)
                     (catch Exception e
                       (.destroy proc)
                       (delete-tree! dir)
                       (throw (ex-info (str "node didn't connect within " timeout "ms") {} e)))
                     (finally (.close server)))
           _ (.setSoTimeout sock timeout)
           in (BufferedReader. (InputStreamReader. (.getInputStream sock) StandardCharsets/UTF_8))
           w (OutputStreamWriter. (.getOutputStream sock) StandardCharsets/UTF_8)
           lines (LinkedBlockingQueue.)
           rt (->NodeRuntime proc sock lines timeout w dir out)]
       (when-not (= "ready" (try (.readLine in) (catch SocketTimeoutException _ nil)))
         (.close ^Closeable rt)
         (throw (ex-info "node didn't say it was ready" {})))
       ;; an idle Node says nothing, and an event may come any time: the
       ;; wait for an answer is run-js's
       (.setSoTimeout sock 0)
       (read-lines! in lines on-event)
       rt))))

(defn close [^Closeable rt] (.close rt))

(defn closed?
  "Whether the runtime was closed, or Node went away."
  [{:keys [^Process proc ^Socket sock]}]
  (or (.isClosed sock) (not (.isAlive proc))))

(defn- read-text [s] (edn/read-string s))

(defn run-js
  "Run `code`, a function expression of the runtime, in Node:
  {:value printed} or {:thrown {:class :message :data printed}}. What the
  code prints is given to `on-out`, a string at a time, by default written
  to the runtime's :out. With `hook`, a JS expression, the code is a
  function of the runtime and of what that evaluates to. Its third argument
  is `tell`, which sends a string to the runtime's :on-event."
  ([rt code file] (run-js rt code file nil nil))
  ([rt code file on-out] (run-js rt code file on-out nil))
  ([{:keys [^LinkedBlockingQueue lines timeout ^Writer w ^Writer out] :as rt} code file on-out hook]
   (locking rt
     (.write w (str "{\"code\": " (emit/string-literal code)
                    ", \"file\": " (emit/string-literal (or file "repl"))
                    (when hook (str ", \"hook\": " (emit/string-literal hook)))
                    "}\n"))
     (.flush w)
     (loop []
       (let [line (.poll lines (long timeout) TimeUnit/MILLISECONDS)
             _ (when-not line
                 (close rt)
                 (throw (ex-info "node didn't answer in time; the runtime is closed" {})))
             _ (when (= ::closed line)
                 (.put lines ::closed)
                 (throw (ex-info "node closed the connection" {})))
             [tag & fields] (str/split line #"\t" -1)]
         (case tag
           "out" (let [^String s (read-text (first fields))]
                   (if on-out (on-out s) (doto out (.write s) (.flush)))
                   (recur))
           "value" {:value (first fields)}
           "thrown" (let [[c m d] fields]
                      {:thrown {:class (read-text c) :message (read-text m) :data d}})))))))

(defn- error-of [record]
  (first (filter #(= :error (:severity %)) (:diagnostics record))))

(defn run-record
  "Compile and run the record of one top-level form, analysed in namespace
  `ns`: {:js code} with :value or :thrown, or {:error diagnostic} when it
  doesn't compile. `on-out` is run-js's. With `rec`, {:site :hook
  :locals?}, the form is compiled to be recorded: its sites are marked
  first, unless there is no :site, the record's being marked already."
  ([rt ns record file] (run-record rt ns record file nil nil))
  ([rt ns record file on-out] (run-record rt ns record file on-out nil))
  ([rt ns record file on-out {:keys [hook] :as rec}]
   (if-let [e (error-of record)]
     {:error e}
     (let [record (cond-> record (:site rec) (sites/mark rec))
           code (try (emit/form-js ns (:node record) (some? rec))
                     (catch clojure.lang.ExceptionInfo e
                       (or (some->> (u/diagnostic e) (hash-map :error))
                           (throw e))))]
       (if (map? code)
         code
         (assoc (run-js rt code file on-out hook) :js code))))))

(defn eval-form
  "Analyse for JS, compile and run the form `form`, already read, in
  namespace `ns`. {:record r :ns ns-after} with :value or :thrown, or
  :error when it doesn't compile. Options are analyze-top's, and
  :record {:site :hook :locals?} to record it."
  [rt cenv form {:keys [ns file record] :as opts}]
  (let [ns (or ns 'user)
        r (a/analyze-top cenv form (assoc opts :ns ns :host (host/js)))]
    (merge {:record r :ns (:ns r)} (run-record rt ns r file nil record))))

(defn load-source
  "Read, analyse, compile and run every form of `src`, in order, each before
  the next is read. Stops at the first form that doesn't compile or throws:
  {:results [{:record ...} ...] :diagnostics reader-diagnostics}."
  [rt cenv src opts]
  (let [results (volatile! [])
        ns (volatile! (or (:ns opts) 'user))
        stop (Object.)
        run (fn [r]
              (let [out (assoc (run-record rt @ns r (:file opts)) :record r)]
                (vswap! results conj out)
                (vreset! ns (:ns r))
                (when (or (:error out) (:thrown out)) (throw (ex-info "stop" {::stop stop})))))
        analysis (try (a/analyze-source cenv src (assoc opts :on-unit run :target :js))
                      (catch clojure.lang.ExceptionInfo e
                        (if (identical? stop (::stop (ex-data e))) nil (throw e))))]
    {:results @results :diagnostics (:diagnostics analysis)}))

(defn remove-ns!
  "Forget namespace `ns` in Node, so it starts afresh."
  [rt ns]
  (run-js rt (str "(function ($rt) { $rt.removeNamespace(" (emit/string-literal (str ns)) "); return null; })")
          "remove-ns"))
