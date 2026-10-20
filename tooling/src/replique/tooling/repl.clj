(ns replique.tooling.repl
  "A repl connection of the server (doc/server.md §3): Replique code read
  from the socket, evaluated form by form on one target, and answered with
  frames.

  A target is the compiler environment and the runtime the code runs in.
  Every repl of a target shares them, so a var one repl defines is the var
  another one calls: a JVM target in this JVM (replique.jvm.dev), a JS target
  in one Node process (replique.js.dev).

  The code arrives a line at a time and is evaluated when what has arrived
  reads to the end; the directives replique.el writes before a form, the
  #replique/... tags, are read here rather than by the reader."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [replique.analyzer :as a]
            [replique.js.dev :as js]
            [replique.jvm.dev :as jvm]
            [replique.reader.cst :as cst]
            [replique.tooling.recording :as recording]
            [replique.tooling.scope :as scope])
  (:import [java.io BufferedReader Writer]
           [replique.lang Core ExceptionInfo Printer]))

;; --- targets ---------------------------------------------------------------

(defn jvm-target
  "The JVM target, whose code compiled to be recorded reports to
  `recorder`, a replique.tooling.recording/recorder."
  [recorder]
  {:id :jvm :rt (jvm/runtime) :cenv (a/environment) :recorder recorder :uses (scope/uses)})

(defn js-target
  "A JS target: Node started, which throws when it can't be, and made to
  record to `recorder`."
  [recorder]
  (let [rt (js/runtime {:on-event #(recording/told! recorder %)})]
    (try (recording/install! rt)
         (catch Throwable t (js/close rt) (throw t)))
    {:id :js :rt rt :cenv (a/environment) :recorder recorder :uses (scope/uses)}))

(defn close-target [{:keys [id rt]}]
  (when (= :js id) (js/close rt)))

;; --- frames ----------------------------------------------------------------

(defn frame
  "A frame of `kvs`, the nil ones left out: a frame's own keys are never
  null (doc/server.md §1)."
  [& kvs]
  (into {} (comp (map vec) (remove (comp nil? second))) (partition 2 kvs)))

(defn- out-writer
  "A Writer turning what is written to it into `out` frames, a line at a time
  on flush, the rest when `drain` is set or it is long."
  ^Writer [send!]
  (let [sb (StringBuilder.)
        emit! (fn [n] (when (pos? n)
                        (send! (frame :tag "out" :string (subs (str sb) 0 n)))
                        (.delete sb 0 (int n))))]
    (proxy [Writer] []
      (write
        ([x]
         (locking sb
           (cond (string? x) (.append sb ^String x)
                 (integer? x) (.append sb (char (int x)))
                 :else (.append sb ^chars x))))
        ([x off len]
         (locking sb
           (if (string? x)
             (.append sb ^String x (int off) (int (+ off len)))
             (.append sb ^chars x (int off) (int len))))))
      (flush []
        (locking sb
          (emit! (if (> (.length sb) 8192)
                   (.length sb)
                   (inc (.lastIndexOf (str sb) "\n"))))))
      (close []
        (locking sb (emit! (.length sb)))))))

(def ^:private trace-limit 64)

(defn- exception-object
  "What an `exception` frame carries about `t`, a JVM throwable."
  [^Throwable t depth]
  (let [trace (.getStackTrace t)
        cause (.getCause t)]
    (frame :class (.getName (class t))
           :message (.getMessage t)
           :data (when (instance? ExceptionInfo t) (Printer/print (.-data ^ExceptionInfo t)))
           :trace (mapv str (take trace-limit trace))
           :trace-dropped (when (> (alength trace) trace-limit) (- (alength trace) trace-limit))
           :cause (when (and cause (< depth 8)) (exception-object cause (inc depth)))
           :cause-dropped (when (and cause (>= depth 8)) true))))

(defn- at [{:keys [file line column]}]
  (str/join ":" (remove nil? [(or file "REPL") line column])))

(defn- diagnostic-failure
  "The failure a diagnostic of the reader or the compiler is."
  [phase {:keys [code message span]}]
  {:phase phase
   :message (str (if (= "read-source" phase) "Syntax error reading source" "Syntax error compiling")
                 " at (" (at span) ").\n" message)
   :exception (frame :class (str (namespace code) "/" (name code)) :message message)})

(defn- thrown-failure [^Throwable t]
  {:phase "execution"
   :message (str "Execution error (" (.getSimpleName (class t)) ").\n" (.getMessage t))
   :exception (exception-object t 0)})

(defn- warning-text [{:keys [message span]}]
  (str "WARNING " message " at (" (at span) ")\n"))

;; --- evaluating ------------------------------------------------------------

(defn- run
  "Run the record `r`, analysed in `ns`: {:value printed} or {:failure f}.
  With `where`, the place of its form, it is recorded."
  [{:keys [id rt recorder] :as target} ns r file ^Writer out where]
  (try
    (let [on-out (fn [^String s] (.write out s) (.flush out))
          {:keys [error value thrown]}
          (if where
            (recording/run-record recorder target ns r where file on-out)
            (let [result (if (= :jvm id)
                           (jvm/run-record rt ns r)
                           (js/run-record rt ns r file on-out))]
              (when-not (:error result)
                (recording/defined! recorder id (:defines r) nil))
              result))
          _ (when-not error (scope/used! target r))]
      (cond
        error {:failure (diagnostic-failure "compilation" error)}
        thrown {:failure {:phase "execution"
                          :message (str "Execution error (" (:class thrown) ").\n" (:message thrown))
                          :exception (frame :class (:class thrown) :message (:message thrown)
                                            :data (:data thrown))}}
        (= :jvm id) (try {:value (Printer/print value)}
                         (catch Throwable t
                           {:failure (assoc (thrown-failure t) :phase "print-eval-result")}))
        :else {:value value}))
    (catch Throwable t
      {:failure (thrown-failure t)})))

(defn- padded
  "`text`, preceded by what puts its first character at `line` and `column`:
  the spans the reader records are then the file's."
  [text line column]
  (str (str/join (repeat (dec (or line 1)) \newline))
       (str/join (repeat (dec (or column 1)) \space))
       text))

(defn- place
  "Where `text`, starting at `line` and `column` of `file`, is: a span."
  [text file line column]
  (let [lines (str/split text #"\n" -1)
        line (or line 1)
        column (or column 1)]
    (frame :file file :line line :column column
           :end-line (+ line (dec (count lines)))
           :end-column (if (next lines) (inc (count (peek lines))) (+ column (count text))))))

(defn evaluate
  "Read, analyse and run the one top-level form of `text`, which starts at
  `line` and `column` of `file`, in namespace `ns`, recorded with `record`
  (doc/recording.md §6). Warnings are written to `out`. {:ns ns-after} with
  :value, the printed value, or :failure."
  [target text {:keys [ns file line column record]} ^Writer out]
  (let [outcome (volatile! nil)
        where (when record (assoc (place text file line column) :text text))
        stop (Object.)
        on-unit (fn [r]
                  (doseq [d (:diagnostics r) :when (= :warning (:severity d))]
                    (.write out ^String (warning-text d)))
                  (.flush out)
                  (vreset! outcome (assoc (run target ns r file out where) :ns (:ns r)))
                  (when (:failure @outcome) (throw (ex-info "stop" {::stop stop}))))
        {:keys [diagnostics] :as analysis}
        (try (a/analyze-source (:cenv target) (padded text line column)
                               {:file file :ns ns :target (:id target) :on-unit on-unit})
             (catch Throwable t
               (if (identical? stop (::stop (ex-data t)))
                 nil
                 {::broken t})))
        read-error (first (filter #(= :error (:severity %)) diagnostics))]
    (cond
      @outcome @outcome
      (::broken analysis) {:ns ns :failure (assoc (thrown-failure (::broken analysis)) :phase "compilation")}
      read-error {:ns ns :failure (diagnostic-failure "read-source" read-error)}
      :else {:ns ns :value "nil"})))

(defn- node-errors [p node]
  (filter #(and (= :error (:severity %)) (<= (:start node) (:start %)) (<= (:start %) (:end node)))
          (:diagnostics p)))

(defn- read-error [p node file]
  (let [d (or (first (node-errors p node))
              {:code :reader/error :message "unreadable" :start (:start node) :end (:end node)})]
    (diagnostic-failure "read-source"
                        (assoc d :span (assoc (cst/span p (:start d) (:end d)) :file file)))))

(def ^:private code-node?
  (complement (comp #{:whitespace :comment :discard} :kind)))

(defn load-file!
  "Evaluate every top-level form of the file `file`, in order, in a namespace
  that starts at user: {:value of the last} or the first :failure."
  [target file out]
  (let [text (slurp file)
        p (cst/parse text {:file file})]
    (loop [nodes (filter code-node? (:nodes p)) ns 'user value "nil"]
      (if-let [n (first nodes)]
        (if (:error? n)
          {:failure (read-error p n file)}
          (let [{:keys [line column]} (cst/span p n)
                r (evaluate target (cst/source-text p n) {:ns ns :file file :line line :column column} out)]
            (if (:failure r)
              r
              (recur (rest nodes) (:ns r) (:value r)))))
        {:value value}))))

;; --- directives ------------------------------------------------------------

(def ^:private directives '#{replique/src replique/ns replique/load replique/reload replique/frame})

(defn- directive-value
  "The EDN value of a directive node's form."
  [p node]
  (let [[form] (cst/forms node)]
    (edn/read-string (cst/source-text p form))))

(defn- refuse [& message]
  (throw (ex-info (apply str message) {::refused true})))

(defn- check-src [v]
  (when-not (map? v) (refuse "#replique/src takes a map"))
  (let [{:keys [file line column instrument record]} v]
    (when (and (some? file) (not (string? file))) (refuse "#replique/src :file is a string"))
    (when (and (some? line) (not (and (int? line) (<= 1 line Integer/MAX_VALUE))))
      (refuse "#replique/src :line is an integer from 1"))
    (when (and (some? column) (not (and (int? column) (<= 1 column Integer/MAX_VALUE))))
      (refuse "#replique/src :column is an integer from 1"))
    (when (and instrument (not (and record (false? (:entry v)))))
      (refuse "#replique/src :instrument needs the debugger, which Replique doesn't have yet:"
              " it records, with :record true :entry false"))
    (when (and record (not (true? record)))
      (refuse "#replique/src :record is true"))
    v))

(defn- check-ns [v]
  (cond (not (symbol? v)) (refuse "#replique/ns takes a namespace's name")
        (namespace v) (refuse (str "#replique/ns takes a namespace's name, not " v))
        :else v))

(defn- check-load [v]
  (when-not (and (map? v) (string? (:file v))) (refuse "#replique/load takes {:file path}"))
  (when (:entry v) (refuse "#replique/load can't load an entry of a jar yet"))
  v)

;; --- the repl --------------------------------------------------------------

(defn- refused-failure [message]
  {:phase "read-source" :message message :exception (frame :class "Directive" :message message)})

(defn serve
  "Run a repl on the reader `in` until it ends or :repl/quit is read.
  `session` is {:connection id :params map :busy atom}, `target-of` a
  function of no arguments giving the target the next form runs on, and
  `send!` writes a frame. The first prompt is the caller's."
  [^BufferedReader in send! {:keys [connection params busy] :as session} target-of]
  (let [out (out-writer send!)
        state (volatile! {:ns 'user :src nil :moved? false})
        prompt! (fn []
                  (vswap! state assoc :moved? false)
                  (send! (frame :tag "prompt" :connection connection :ns (str (:ns @state))
                                :params params)))
        answer! (fn [{:keys [value failure ns]}]
                  (.close out)
                  (if failure
                    (send! (frame :tag "exception" :phase (:phase failure) :message (:message failure)
                                  :exception (:exception failure)))
                    (send! (frame :tag "ret" :ns (str (or ns (:ns @state))) :value value)))
                  (prompt!))
        busily (fn [f]
                 (reset! busy true)
                 (try (f)
                      (finally (reset! busy false) (Thread/interrupted))))
        form! (fn [p node]
                (let [{:keys [src ns]} @state
                      target (target-of)
                      r (busily #(evaluate target (cst/source-text p node)
                                           {:ns ns :file (:file src) :line (:line src) :column (:column src)
                                            :record (:record src)}
                                           out))]
                  (vswap! state assoc :src nil :ns (:ns r))
                  (answer! r)))
        directive! (fn [p node]
                     (let [v (directive-value p node)]
                       (case (:tag node)
                         replique/src (vswap! state assoc :src (check-src v))
                         ;; nothing to create: a namespace exists once a form is read in it
                         replique/ns (vswap! state assoc :ns (check-ns v) :moved? true)
                         replique/load (let [{:keys [file]} (check-load v)]
                                         (vswap! state assoc :src nil)
                                         (answer! (busily #(load-file! (target-of) file out))))
                         replique/reload (refuse "#replique/reload isn't there yet: load the file that changed")
                         replique/frame (refuse "#replique/frame needs the debugger, which Replique doesn't have yet"))))
        node! (fn [p node]
                (cond
                  (:error? node) (do (vswap! state assoc :src nil)
                                     (answer! {:failure (read-error p node nil)}))
                  (and (= :tagged (:kind node)) (directives (:tag node)))
                  (try (directive! p node)
                       (catch Exception e
                         (vswap! state assoc :src nil)
                         (answer! {:failure (refused-failure
                                             (if (::refused (ex-data e))
                                               (ex-message e)
                                               (str "Unreadable " (:tag node) ": " (ex-message e))))})))
                  (= ":repl/quit" (cst/source-text p node)) ::quit
                  :else (form! p node)))]
    (Core/setOut out)
    (try
      (loop [buffer ""]
        (let [line (.readLine in)
              text (if line (str buffer line "\n") buffer)]
          (cond
            (and (nil? line) (str/blank? text)) nil
            (str/blank? text) (do (when (:moved? @state) (prompt!)) (recur ""))
            :else
            (let [p (cst/parse text {})]
              (if (and line (cst/incomplete? p))
                (recur text)
                (when-not (some #(= ::quit (node! p %)) (filter code-node? (:nodes p)))
                  (when line (recur ""))))))))
      (finally (Core/setOut nil)))))
