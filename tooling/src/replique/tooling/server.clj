(ns replique.tooling.server
  "The REPL server replique.el talks to (doc/server.md): the part of the
  replique protocol a Replique repl needs, on one TCP port.

    clojure -M:server '{:process-id \"my-app\"}'

    (def s (start {:directory \"/home/me/my-app\"}))
    (:port (:description s))
    (stop s)

  A connection is a control connection or a repl, as its handshake says.
  Clients write EDN and the server writes JSON, one frame per line. A repl
  evaluates Replique on the JVM, in this JVM, or in JS, in one Node process
  started when the first JS repl opens; the repls of a target share its
  namespaces."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [replique.js.dev :as js]
            [replique.tooling.inspect :as inspect]
            [replique.tooling.json :as json]
            [replique.tooling.recorded :as recorded]
            [replique.tooling.recording :as recording]
            [replique.tooling.repl :as repl]
            [replique.tooling.scope :as scope])
  (:import [java.io BufferedReader File InputStreamReader OutputStreamWriter PushbackReader
            StringReader Writer]
           [java.net InetAddress ServerSocket Socket SocketException]
           [java.nio.charset StandardCharsets]))

(def version "0.1.0-SNAPSHOT")

(def protocol-version 1)

;; --- messages --------------------------------------------------------------

(defn- unknown-tag [tag v]
  {:replique/unknown-tag (str tag) :replique/value v})

(defn- read-messages
  "The EDN messages of `line`: [messages error], error the message of what
  couldn't be read, after which the rest of the line is dropped."
  [^String line]
  (let [r (PushbackReader. (StringReader. line))]
    (loop [out []]
      (let [m (try (edn/read {:eof ::eof :default unknown-tag} r)
                   (catch Exception e {::malformed (ex-message e)}))]
        (cond
          (= ::eof m) [out nil]
          (::malformed m) [out (::malformed m)]
          :else (recur (conj out m)))))))

(defn- id-of [m]
  (let [id (:id m)]
    (when (or (string? id) (int? id)) id)))

(defn- error-frame [m kind message & kvs]
  (apply repl/frame :tag "error" :id (id-of m) :op (some-> (:op m) name)
         :error kind :message message kvs))

(defn- reply [m & kvs]
  (apply repl/frame :tag "reply" :id (id-of m) :op (name (:op m)) kvs))

;; --- the server ------------------------------------------------------------

(defn- sender
  "A function writing a frame to `w`, a line at a time; frames from several
  threads keep their order."
  [^Writer w]
  (fn [frame]
    (locking w
      (.write w (json/write-str frame))
      (.write w "\n")
      (.flush w))))

(defn- description-frame [s & kvs]
  (merge (:description s) (apply repl/frame kvs)))

(defn- js-target
  "The JS target, Node started the first time; started again when it went
  away, with a fresh compiler environment, since what Node held is gone."
  [{:keys [js-target recorder]}]
  (locking js-target
    (let [t @js-target]
      (if (and t (not (js/closed? (:rt t))))
        t
        (do (when t (recording/runtime-gone! recorder (:rt t)))
            (reset! js-target (repl/js-target recorder)))))))

(defn- target-of [s target]
  (if (= :js target)
    #(js-target s)
    (constantly (:jvm-target s))))

(def ^:private param-names [:print-length :print-level :print-meta :warn-on-reflection])

(defn- repl-hello
  "The target and printing params a repl handshake asks for, or the error
  frame refusing it."
  [m]
  (let [{:keys [dialect target main params frame]} m
        target (case dialect
                 (nil :replique :clj) (case target (nil :jvm) :jvm (:js :node) :js ::invalid)
                 :cljs (case target (nil :node :js) :js ::invalid)
                 ::invalid-dialect)]
    (cond
      (= ::invalid-dialect target)
      (error-frame m "invalid-dialect" (str "This is a Replique server: a repl's :dialect is :replique,"
                                            " or :clj or :cljs as replique.el spells the JVM and JS")
                   :dialects ["replique" "clj" "cljs"])
      (= ::invalid target)
      (error-frame m "invalid-target" (str "A Replique repl runs on :jvm or :js, in Node, not "
                                           (pr-str (:target m))))
      main (error-frame m "invalid-main" "A Replique repl can't be started on a namespace yet")
      frame (error-frame m "invalid-frame" "Replique has no debugger yet")
      (not (or (nil? params)
               (and (map? params) (every? (set param-names) (keys params)))))
      (error-frame m "invalid-params" "The printing params are these four" :params param-names)
      :else {:target target
             :params (merge {:print-length nil :print-level nil :print-meta false
                             :warn-on-reflection false}
                            params)})))

(declare stop)

(defn- broadcast!
  "Send the event `frame` to every control connection."
  [conns frame]
  (doseq [{:keys [send!]} (vals @conns) :when send!]
    (try (send! frame) (catch Exception _ nil))))

(defn- recording-op
  "The reply to `m`, an op answered by `f` of the recorder and `m`, or the
  error refusing it."
  [s m f]
  (try (apply reply m (mapcat identity (f (:recorder s) m)))
       (catch clojure.lang.ExceptionInfo e
         (if-let [kind (recording/refused e)]
           (error-frame m (name kind) (ex-message e))
           (throw e)))))

(defn- js-rt
  "The Node of the JS target, where it is running."
  [s]
  (let [t @(:js-target s)]
    (when (and t (not (js/closed? (:rt t)))) (:rt t))))

(defn- inspect-op
  "The reply to `m`, an op of the inspector answered by `f` of the server's
  views and `m`, or the error refusing it."
  [s m f]
  ;; what a value does when it is printed is the value's: a failure is
  ;; answered, and the connection goes on
  (try (recording-op s m (fn [_ m] (f (:views s) m)))
       (catch Exception e
         (error-frame m "exception" (or (ex-message e) (.getName (class e)))))))

(defn- target-asked
  "The target the dialect keys of `m` name, as a repl's handshake does: the
  JS one only where Node is running."
  [s m]
  (if (or (= :cljs (:dialect m)) (#{:js :node} (:target m)))
    (or (let [t @(:js-target s)] (when (and t (not (js/closed? (:rt t)))) t))
        (recording/refuse :no-runtime "No node runtime is running"))
    (:jvm-target s)))

(defn- control-op [s conn m]
  (case (:op m)
    :process-info (description-frame s :tag "reply" :id (id-of m) :op "process-info"
                                     :connection (:id conn)
                                     :uptime (- (System/currentTimeMillis)
                                                (get-in s [:description :started-at])))
    :interrupt (let [target (get @(:conns s) (:connection m))]
                 (cond
                   (not (string? (:connection m)))
                   (error-frame m "invalid-message" ":interrupt names a :connection")
                   (not target) (error-frame m "unknown-connection" (str "No connection " (:connection m)))
                   (not (:busy target)) (error-frame m "not-a-repl" (str (:connection m) " is not a repl"))
                   :else (let [busy? (and @(:busy target) (= :jvm (:target target)))]
                           (when busy? (.interrupt ^Thread (:thread target)))
                           (reply m :connection (:connection m) :interrupted busy?))))
    :debug-recorded (recording-op s m recorded/recorded)
    :debug-cursor (recording-op s m recorded/cursor)
    :debug-calls (recording-op s m recorded/calls)
    :debug-find-iteration (recording-op s m #(recorded/find-iteration
                                              %1 (fn [id] (if (= :js id) @(:js-target s) (:jvm-target s)))
                                              %2))
    :debug-uninstrument (recording-op s m recording/uninstrument)
    :recording-scope (recording-op s m #(scope/recording-scope (target-asked s %2)
                                                               (get-in s [:description :directory]) %2))
    :record-calls (recording-op s m #(scope/record-calls %1 (target-asked s %2)
                                                         (get-in s [:description :directory]) %2))
    :inspect (inspect-op s m #(inspect/inspect %1 (:recorder s) (fn [] (js-rt s)) (:id conn) %2))
    :inspect-refresh (inspect-op s m inspect/refresh)
    :inspect-children (inspect-op s m inspect/children)
    :inspect-print (inspect-op s m inspect/printed)
    :inspect-path (inspect-op s m inspect/path)
    :inspect-def (inspect-op s m #(inspect/def! %1 (:cenv (:jvm-target s)) %2))
    :inspect-close (inspect-op s m inspect/close!)
    ;; the reply is given a head start: the connection closing is what says
    ;; the server went
    :shutdown (do (future (Thread/sleep 100) (stop s))
                  (reply m :stopping true))
    (error-frame m "unknown-op" (str "This server doesn't answer " (pr-str (:op m)) " yet"))))

(defn- control-loop [s conn ^BufferedReader in send!]
  (loop []
    (when-let [line (.readLine in)]
      (let [[ms error] (read-messages line)]
        (doseq [m ms]
          (send! (if (and (map? m) (keyword? (:op m)))
                   (control-op s conn m)
                   (error-frame (when (map? m) m) "invalid-message" "A message is a map with an :op"))))
        (when error (send! (repl/frame :tag "error" :error "malformed-message" :message error))))
      (recur))))

(defn- handshake
  "The :hello of a connection's first line: the message, or the error frame
  refusing it."
  [s line]
  (let [[ms error] (if line (read-messages line) [[] "the connection ended"])
        m (first ms)]
    (cond
      error (repl/frame :tag "error" :error "malformed-message" :message error)
      (not (and (map? m) (= :hello (:op m))))
      (error-frame (when (map? m) m) "expected-hello" "The first message is a :hello")
      (next ms) (error-frame m "invalid-message" "The :hello is alone on its line")
      (and (:process-id m) (not= (:process-id m) (get-in s [:description :process-id])))
      (error-frame m "process-id-mismatch" (str "This is " (get-in s [:description :process-id])))
      (not (#{:control :repl} (:role m)))
      (error-frame m "invalid-role" "The :role is :control or :repl")
      :else m)))

(defn- connection [s ^Socket sock]
  (let [in (BufferedReader. (InputStreamReader. (.getInputStream sock) StandardCharsets/UTF_8))
        send! (sender (OutputStreamWriter. (.getOutputStream sock) StandardCharsets/UTF_8))
        id (str "c" (swap! (:counter s) inc))]
    (try
      (let [m (handshake s (.readLine in))]
        (if (= "error" (:tag m))
          (send! m)
          (case (:role m)
            :control
            ;; told events once the reply is sent, which comes first
            (do (send! (description-frame s :tag "reply" :id (id-of m) :op "hello"
                                          :role "control" :connection id))
                (swap! (:conns s) assoc id {:id id :socket sock :send! send!})
                (control-loop s {:id id} in send!))

            :repl
            (let [{:keys [target params] :as hello} (repl-hello m)]
              (if (= "error" (:tag hello))
                (send! hello)
                (let [target-of (target-of s target)
                      ;; Node started before the reply, or the repl refused
                      no-runtime (try (target-of) nil
                                      (catch Exception e (error-frame m "no-runtime" (ex-message e))))
                      busy (atom false)]
                  (if no-runtime
                    (send! no-runtime)
                    (do
                      (swap! (:conns s) assoc id {:id id :socket sock :thread (Thread/currentThread)
                                                  :busy busy :target target})
                      (send! (description-frame s :tag "reply" :id (id-of m) :op "hello" :role "repl"
                                                :connection id :dialect "replique" :target (name target)))
                      (send! (repl/frame :tag "prompt" :connection id :ns "user" :params params))
                      (repl/serve in send! {:connection id :params params :busy busy} target-of)))))))))
      (catch SocketException _ nil)
      (catch Exception e
        (try (send! (repl/frame :tag "error" :error "exception" :message (ex-message e)))
             (catch Exception _ nil)))
      (finally
        (swap! (:conns s) dissoc id)
        (inspect/closed! (:views s) id)
        (.close sock)))))

(defn- thread [name f]
  (doto (Thread. ^Runnable f ^String name) (.setDaemon true) (.start)))

(defn- port-file ^File [{:keys [directory port-file process-id]}]
  (if port-file
    (let [f (io/file port-file)] (if (.isAbsolute f) f (io/file directory port-file)))
    (io/file directory ".replique" "processes" (str process-id ".json"))))

(def ^:private option-names #{:process-id :host :port :directory :port-file})

(defn start
  "Start a server. Options, all optional:

    :process-id  the name clients know it by, a random UUID by default
    :host        \"127.0.0.1\"
    :port        0, a free port
    :directory   the working directory
    :port-file   <directory>/.replique/processes/<process-id>.json

  The port file is where replique.el finds the server; it is written with
  the server's description and removed by stop. A server whose port file
  already exists doesn't start."
  ([] (start {}))
  ([opts]
   (when-let [bad (seq (remove option-names (keys opts)))]
     (throw (ex-info (str "Unknown options " (vec bad)) {:options (vec bad)})))
   (let [{:keys [process-id host port directory] :as opts}
         (merge {:process-id (str (random-uuid)) :host "127.0.0.1" :port 0
                 :directory (System/getProperty "user.dir")}
                opts)
         _ (when-not (and (string? process-id) (re-matches #"[A-Za-z0-9][A-Za-z0-9._+-]{0,127}" process-id))
             (throw (ex-info (str "Invalid :process-id " (pr-str process-id)) {})))
         directory (.getCanonicalPath (io/file directory))
         opts (assoc opts :directory directory)
         pf (port-file opts)
         _ (when (.exists pf)
             (throw (ex-info (str "A process named " process-id " is already running here: " pf
                                  " exists. Delete it if nothing answers on its port.")
                             {:port-file (str pf)})))
         server (ServerSocket. (int port) 50 (InetAddress/getByName host))
         description {:protocol-version protocol-version
                      :replique-version version
                      :process-id process-id
                      :host host
                      :port (.getLocalPort server)
                      :directory directory
                      :pid (.pid (java.lang.ProcessHandle/current))
                      :java-version (System/getProperty "java.version")
                      :language "replique"
                      :targets ["jvm" "js"]
                      :started-at (System/currentTimeMillis)}
         conns (atom {})
         recorder (recording/recorder #(broadcast! conns %))
         s {:description description
            :server server
            :port-file pf
            :conns conns
            :counter (atom 0)
            :recorder recorder
            :views (inspect/views)
            :jvm-target (repl/jvm-target recorder)
            :js-target (atom nil)
            :stopped (promise)}]
     (io/make-parents pf)
     (spit pf (json/write-str (assoc description :tag "started")))
     (thread "replique-server"
             (fn []
               (try
                 (loop []
                   (let [sock (.accept server)]
                     (thread "replique-connection" #(connection s sock))
                     (recur)))
                 (catch SocketException _ nil))))
     s)))

(defn stop
  "Stop the server: close its port and its connections, stop Node, remove
  the port file."
  [{:keys [^ServerSocket server ^File port-file js-target conns stopped]}]
  (.close server)
  (doseq [{:keys [^Socket socket]} (vals @conns)] (.close socket))
  (when-let [t @js-target] (repl/close-target t))
  (.delete port-file)
  (deliver stopped true)
  nil)

(defn -main
  "Start a server with the options of the EDN map argument, print its
  description on one line, and run until :shutdown."
  [& [opts]]
  (let [s (start (if opts (edn/read-string opts) {}))]
    (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable #(.delete ^File (:port-file s))))
    ;; the started line: what a client that spawned the server reads
    (println (json/write-str (assoc (:description s) :tag "started")))
    (flush)
    @(:stopped s)
    (shutdown-agents)
    (System/exit 0)))
