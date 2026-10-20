(ns replique.tooling.server-test
  "The REPL server, through its sockets, as replique.el sees it. The JS
  repls are skipped when node isn't installed."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [replique.tooling.server :as server]
            [replique.tooling.json :as json])
  (:import [java.io BufferedReader File InputStreamReader OutputStreamWriter Writer]
           [java.net Socket]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private node?
  (delay (try (zero? (:exit (sh/sh "node" "--version")))
              (catch java.io.IOException _ false))))

(def ^:dynamic ^:private *server* nil)

(defn- temp-dir ^File []
  (.toFile (Files/createTempDirectory "replique-server-test" (make-array FileAttribute 0))))

(use-fixtures :once
  (fn [t]
    (let [s (server/start {:process-id "test" :directory (str (temp-dir))})]
      (try (binding [*server* s] (t))
           (finally (server/stop s))))))

(defn- connect
  "A client connection: {:in :w :sock}, the first line `hello` sent."
  [hello]
  (let [sock (doto (Socket. "127.0.0.1" (int (:port (:description *server*))))
               (.setSoTimeout 30000))
        c {:sock sock
           :in (BufferedReader. (InputStreamReader. (.getInputStream sock) StandardCharsets/UTF_8))
           :w (OutputStreamWriter. (.getOutputStream sock) StandardCharsets/UTF_8)}]
    (doto ^Writer (:w c) (.write (str (pr-str hello) "\n")) (.flush))
    c))

(defn- frame [{:keys [^BufferedReader in]}]
  (some-> (.readLine in) json/read-str))

(defn- send-text [{:keys [^Writer w]} text]
  (doto w (.write ^String text) (.flush)))

(defn- until-prompt
  "The frames up to and including the next prompt."
  [c]
  (loop [out []]
    (let [f (frame c)]
      (cond (nil? f) out
            (= "prompt" (:tag f)) (conj out f)
            :else (recur (conj out f))))))

(defn- repl
  "A repl connection past its handshake and first prompt."
  [hello]
  (let [c (connect (merge {:op :hello :role :repl :id 1} hello))
        h (frame c)]
    (assert (= "reply" (:tag h)) (pr-str h))
    (until-prompt c)
    (assoc c :hello h)))

(defn- eval-text
  "Send `text` and read the frames of `n` prompts."
  ([c text] (eval-text c text 1))
  ([c text n]
   (send-text c text)
   (vec (mapcat (fn [_] (until-prompt c)) (range n)))))

(defn- rets [frames]
  (mapv #(case (:tag %) "ret" (:value %) "exception" [:exception (:phase %)] (:tag %))
        (remove #(#{"prompt" "out"} (:tag %)) frames)))

(defn- close [{:keys [^Socket sock]}] (.close sock))

(deftest handshake
  (let [c (connect {:op :hello :role :control :id 1})
        h (frame c)]
    (is (= ["reply" "hello" "control" "test" "replique"]
           ((juxt :tag :op :role :process-id :language) h)))
    (is (re-matches #"c\d+" (:connection h)))
    (testing "process-info and an op this server doesn't answer"
      (send-text c "{:op :process-info :id 2} {:op :stale :id \"three\"}\n")
      (is (= ["reply" 2 "test"] ((juxt :tag :id :process-id) (frame c))))
      (is (= ["error" "three" "unknown-op"] ((juxt :tag :id :error) (frame c)))))
    (testing "a malformed message is reported, and the connection survives"
      (send-text c "{:op\n{:op :process-info :id 4}\n")
      (is (= "malformed-message" (:error (frame c))))
      (is (= 4 (:id (frame c)))))
    (close c))
  (testing "refused handshakes close the connection"
    (doseq [[hello kind] [[{:op :process-info} "expected-hello"]
                          [{:op :hello :role :control :process-id "other"} "process-id-mismatch"]
                          [{:op :hello :role :admin} "invalid-role"]
                          [{:op :hello :role :repl :dialect :python} "invalid-dialect"]
                          [{:op :hello :role :repl :dialect :cljs :target :browser} "invalid-target"]
                          [{:op :hello :role :repl :params {:print-width 3}} "invalid-params"]]]
      (let [c (connect hello)]
        (is (= kind (:error (frame c))) (pr-str hello))
        (is (nil? (frame c)))))))

(deftest port-file
  (let [f (io/file (get-in *server* [:description :directory]) ".replique" "processes" "test.json")]
    (is (.exists f))
    (is (= (:port (:description *server*)) (:port (json/read-str (slurp f)))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"already running"
                          (server/start {:process-id "test" :directory (get-in *server* [:description :directory])})))))

(defn- redefinition [hello]
  (let [c (repl hello)]
    (is (= "replique" (:dialect (:hello c))))
    (is (= ["nil" "nil" "1" "nil" "2"]
           (rets (eval-text c "(defn f [] 1)\n(defn g [] (f))\n(g)\n(defn f [] 2)\n(g)\n" 5)))
        "a caller sees a redefinition")
    c))

(deftest jvm-repl
  (let [c (redefinition {})]
    (is (= "jvm" (:target (:hello c))))
    (testing "the prompt says the namespace and the params"
      (let [[ret prompt] (eval-text c "(+ 1 2)\n")]
        (is (= {:tag "ret" :ns "user" :value "3"} ret))
        (is (= ["prompt" "user" (:connection (:hello c))] ((juxt :tag :ns :connection) prompt)))
        (is (= false (:print-meta (:params prompt))))))
    (testing "output comes before the value, a line a frame"
      (is (= [{:tag "out" :string "a 1\n"} {:tag "out" :string "b"} {:tag "ret" :ns "user" :value "nil"}]
             (butlast (eval-text c "(do (println \"a\" 1) (print \"b\"))\n")))))
    (testing "a form over several lines, and two on one"
      (is (= ["3"] (rets (eval-text c "(+ 1\n   2)\n"))))
      (is (= ["1" "2"] (rets (eval-text c "1 2\n" 2)))))
    (testing "blank lines are answered by nothing"
      (is (= ["4"] (rets (eval-text c "\n\n4\n")))))
    (testing "failures: execution, compilation, reading"
      (let [[e] (eval-text c "(throw (ex-info \"boom\" 7))\n")]
        (is (= ["exception" "execution" "replique.lang.ExceptionInfo" "7"]
               ((juxt :tag :phase (comp :class :exception) (comp :data :exception)) e)))
        (is (str/includes? (:message e) "boom")))
      (let [[e] (eval-text c "(nope)\n")]
        (is (= "compilation" (:phase e)))
        (is (str/includes? (:message e) "nope is neither a local nor a var in user")))
      (is (= [[:exception "read-source"]] (rets (eval-text c "(1 2]\n"))))
      (is (= ["5"] (rets (eval-text c "5\n"))) "the repl goes on"))
    (close c)))

(deftest directives
  (let [c (repl {})
        dir (get-in *server* [:description :directory])]
    (testing "#replique/ns moves the repl, and answers alone with a prompt"
      (is (= [{:tag "prompt" :ns "app.core"}]
             (map #(select-keys % [:tag :ns]) (eval-text c "#replique/ns app.core\n\n"))))
      (is (= [["nil" "app.core"] ["1" "app.core"]]
             (->> (eval-text c "(def x 1)\nx\n" 2) (filter :value) (map (juxt :value :ns))))))
    (testing "#replique/ns with a form after it is one prompt"
      (is (= [{:tag "ret" :ns "app.util" :value "nil"} "app.util"]
             (let [[r p] (eval-text c "#replique/ns app.util\n(def y 2)\n")] [r (:ns p)]))))
    (testing "#replique/src puts the form in the file, at its line"
      (let [[e] (eval-text c "#replique/src {:file \"/src/app/util.rpq\" :line 42}\n(nope)\n")]
        (is (str/includes? (:message e) "/src/app/util.rpq:42:2")))
      (let [[e] (eval-text c "(nope)\n")]
        (is (str/includes? (:message e) "REPL:1:2") "for the next form only")))
    (testing "#replique/load loads a file, and leaves the repl where it was"
      (let [f (io/file dir "app" "lib.rpq")]
        (io/make-parents f)
        (spit f "(ns app.lib)\n(defn h [] :one)\n(h)\n")
        (is (= [{:tag "ret" :ns "app.util" :value ":one"}]
               (butlast (eval-text c (str "#replique/load {:file " (pr-str (str f)) "}\n")))))
        (is (= [":one"] (rets (eval-text c "(app.lib/h)\n"))))
        (spit f "(ns app.lib)\n\n(defn h [] (nope))\n")
        (let [[e] (eval-text c (str "#replique/load {:file " (pr-str (str f)) "}\n"))]
          (is (str/includes? (:message e) (str f ":3:13")) (:message e)))))
    (testing "what this server can't do yet is said"
      (is (= [[:exception "read-source"]] (rets (eval-text c "#replique/reload {}\n"))))
      (is (= [[:exception "read-source"]] (rets (eval-text c "#replique/ns a/b\n"))))
      (is (= [[:exception "read-source"]] (rets (eval-text c "#replique/src {:line 0}\n")))))
    (testing ":repl/quit ends the repl"
      (send-text c ":repl/quit\n")
      (is (nil? (frame c))))))

(defn- control
  "A control connection past its handshake."
  []
  (let [c (connect {:op :hello :role :control :id 1})]
    (frame c)
    c))

(defn- ask
  "Send the op `m` on the control connection `c`: its answer, and the
  events before it."
  [c m]
  (send-text c (str (pr-str m) "\n"))
  (loop [events []]
    (let [f (frame c)]
      (if (= "event" (:tag f))
        (recur (conj events f))
        (assoc f :events events)))))

(defn- event
  "The next event `c` is told of, `name`, the others passed by."
  [c name]
  (loop []
    (let [f (frame c)]
      (cond (nil? f) nil
            (= name (:event f)) f
            :else (recur)))))

(def ^:private recorded-src
  (str "#replique/src {:file \"/src/rec.rpq\" :line 3 :column 1"
       " :instrument true :entry false :record true}\n"
       "(defn rf [m]\n  (let [x (inc m)]\n    (* x 2)))\n"))

(deftest recording
  (let [r (repl {})
        c (control)]
    (eval-text r "#replique/ns rec.core\n\n")
    (is (= ["nil"] (rets (eval-text r recorded-src))))
    (let [{:keys [unit] :as e} (event c "debug-instrumented")]
      (is (= {:ns "rec.core" :file "/src/rec.rpq" :line 3 :column 1 :end-line 5 :end-column 14
              :record true}
             (dissoc e :tag :event :unit)))
      (testing "nothing called, nothing recorded"
        (is (= {:calls [] :loops [] :values []}
               (select-keys (ask c {:op :debug-recorded :id 2 :unit unit}) [:calls :loops :values]))))
      (is (= ["4"] (rets (eval-text r "(rf 1)\n"))))
      (is (= {:unit unit :calls 1} (select-keys (event c "debug-recorded") [:unit :calls])))
      (is (= ["6"] (rets (eval-text r "(rf 2)\n"))))
      (let [{:keys [calls call values]} (ask c {:op :debug-recorded :id 3 :unit unit})]
        (is (= 2 (count calls)))
        (is (= call (:call (peek calls))) "the last call")
        (is (= "replique-connection" (:thread (first calls))) "the repl's thread")
        (testing "each form with its value, the entry with its args; x is (inc m)'s"
          (is (= [{:value "3" :times 1 :line 4 :column 11 :end-line 4 :end-column 18}
                  {:value "6" :times 1 :line 5 :column 5 :end-line 5 :end-column 12}
                  {:value "6" :times 1 :line 4 :column 3 :end-line 5 :end-column 13}
                  {:kind "enter" :params ["m"] :args ["2"] :value "#array[2]" :times 1
                   :line 3 :column 1 :end-line 5 :end-column 14}]
                 (mapv #(dissoc % :site) values)))))
      (testing "a call before it, and one not kept"
        (let [{:keys [calls]} (ask c {:op :debug-recorded :id 4 :unit unit})
              first-call (:call (first calls))]
          (is (= "4" (:value (first (filter #(= 5 (:line %))
                                            (:values (ask c {:op :debug-recorded :id 5 :unit unit
                                                             :call first-call})))))))
          (is (= "unknown-call" (:error (ask c {:op :debug-recorded :id 6 :unit unit :call 999999}))))))
      (testing "defined again, it isn't recorded any more"
        (eval-text r "(defn rf [m] m)\n")
        (is (= unit (:unit (event c "debug-uninstrumented"))))
        (is (= "unknown-unit" (:error (ask c {:op :debug-recorded :id 7 :unit unit}))))))
    (testing "recorded again, and let go of"
      (eval-text r recorded-src)
      (let [{:keys [unit]} (event c "debug-instrumented")
            {:keys [uninstrumented events]} (ask c {:op :debug-uninstrument :id 8})]
        (is (= [unit] uninstrumented))
        (is (= [unit] (map :unit events)) "and told")))
    (testing "stepping through needs the debugger"
      (let [[e] (eval-text r "#replique/src {:line 1 :instrument true}\n(+ 1 2)\n")]
        (is (= "read-source" (:phase e)))
        (is (str/includes? (:message e) "debugger"))))
    (close r)
    (close c)))

(deftest js-recording
  (if-not @node?
    (println "node isn't installed: skipping JS recording")
    (let [r (repl {:target :js})
          c (control)]
      (eval-text r "#replique/ns rec.core\n\n")
      (is (= ["nil"] (rets (eval-text r recorded-src))))
      (let [{:keys [unit] :as e} (event c "debug-instrumented")]
        (is (= {:ns "rec.core" :file "/src/rec.rpq" :line 3 :column 1 :end-line 5 :end-column 14
                :record true}
               (dissoc e :tag :event :unit)))
        (testing "nothing called, nothing recorded: the definition's evaluation is no call"
          (is (= {:calls [] :loops [] :values []}
                 (select-keys (ask c {:op :debug-recorded :id 2 :unit unit}) [:calls :loops :values]))))
        (is (= ["4"] (rets (eval-text r "(rf 1)\n"))))
        (is (= {:unit unit :calls 1} (select-keys (event c "debug-recorded") [:unit :calls]))
            "Node tells")
        (is (= ["6"] (rets (eval-text r "(rf 2)\n"))))
        (let [{:keys [calls call values]} (ask c {:op :debug-recorded :id 3 :unit unit})]
          (is (= 2 (count calls)))
          (is (= call (:call (peek calls))))
          (is (not (contains? (first calls) :thread)) "Node has one thread, and says none")
          (is (integer? (:at (first calls))))
          (is (= [{:value "3" :times 1 :line 4 :column 11 :end-line 4 :end-column 18}
                  {:value "6" :times 1 :line 5 :column 5 :end-line 5 :end-column 12}
                  {:value "6" :times 1 :line 4 :column 3 :end-line 5 :end-column 13}
                  {:kind "enter" :params ["m"] :args ["2"] :value "#array[2]" :times 1
                   :line 3 :column 1 :end-line 5 :end-column 14}]
                 (mapv #(dissoc % :site) values))
              "the same as on the JVM")
          (testing "a call before it, and one not kept"
            (is (= "4" (:value (first (filter #(= 5 (:line %))
                                              (:values (ask c {:op :debug-recorded :id 4 :unit unit
                                                               :call (:call (first calls))})))))))
            (is (= "unknown-call" (:error (ask c {:op :debug-recorded :id 5 :unit unit :call 999999}))))))
        (testing "a callback records in the call it was made in, once that is over"
          (eval-text r (str "#replique/src {:file \"/src/cb.rpq\" :line 1 :column 1"
                            " :instrument true :entry false :record true}\n"
                            "(defn later [x] (js/setTimeout (fn [] (inc x)) 10))\n"))
          (let [{u :unit} (event c "debug-instrumented")]
            (eval-text r "(later 41)\n")
            (Thread/sleep 300)
            (is (some #(= "42" (:value %)) (:values (ask c {:op :debug-recorded :id 6 :unit u}))))))
        (testing "a JVM definition of the same name is another var"
          (let [j (repl {})]
            (eval-text j "#replique/ns rec.core\n(defn rf [m] m)\n")
            (is (= 2 (count (:calls (ask c {:op :debug-recorded :id 7 :unit unit})))))
            (close j)))
        (testing "defined again, it isn't recorded any more"
          (eval-text r "(defn rf [m] m)\n")
          (is (= unit (:unit (event c "debug-uninstrumented"))))
          (is (= "unknown-unit" (:error (ask c {:op :debug-recorded :id 8 :unit unit}))))))
      (let [{:keys [uninstrumented]} (ask c {:op :debug-uninstrument :id 9})]
        (is (seq uninstrumented)))
      (close r)
      (close c))))

(defn- src
  "`text` recorded, from the line `line` of /src/go.rpq."
  [line text]
  (str "#replique/src {:file \"/src/go.rpq\" :line " line " :column 1"
       " :instrument true :entry false :record true}\n" text))

(defn- at-place
  "Of `values`, as `:debug-recorded` says them, the one at `line` and
  `column`."
  [values line column]
  (first (filter #(and (= line (:line %)) (= column (:column %))) values)))

(defn- going-through
  "Loops and iterations, the cursor and the calls tree, on the target of a
  repl opened with `hello`: the same answers on both."
  [hello]
  (let [r (repl hello)
        c (control)
        unit! #(:unit (event c "debug-instrumented"))
        ask! (let [id (atom 100)] #(ask c (assoc % :id (swap! id inc))))]
    (eval-text r "#replique/ns go.core\n\n")
    (eval-text r (src 1 (str "(defn lp [n]\n  (loop [i 0 acc 0]\n    (if (< i n)\n"
                             "      (recur (inc i) (+ acc (* i 2)))\n      acc)))\n")))
    (let [lp (unit!)]
      (is (= ["6"] (rets (eval-text r "(lp 3)\n"))))
      (let [{:keys [loops values]} (ask! {:op :debug-recorded :unit lp})
            i (:site (at-place values 2 10))
            twice (:site (at-place values 4 29))]
        (testing "a loop, the first name it binds each time round, and how many times"
          (is (= [{:site i :iterations 4 :kept [[1 4]] :line 2 :column 3 :end-line 5 :end-column 12}]
                 loops))
          (is (= {:kind "local" :value "3" :times 4} (select-keys (at-place values 2 10) [:kind :value :times]))))
        (testing "one iteration: what the loop evaluated in it, and the rest as in the call"
          (let [{:keys [values iteration]} (ask! {:op :debug-recorded :unit lp :iteration {:site i :index 2}})]
            (is (= {:site i :index 2} iteration))
            (is (= [["1" 1] ["0" 1] ["true" 1] ["2" 1] ["2" 1] ["2" 1]]
                   (map (juxt :value :times) (filter #(<= 2 (:line %) 4) (remove #(= 3 (:column %)) values)))))
            (is (nil? (at-place values 3 5)) "the if has no value then: the loop went round")
            (is (= "6" (:value (at-place values 2 3))) "the loop isn't in itself"))
          (is (= "false" (:value (at-place (:values (ask! {:op :debug-recorded :unit lp
                                                           :iteration {:site i :index 4}}))
                                           3 9))))
          (is (= "unknown-iteration" (:error (ask! {:op :debug-recorded :unit lp :iteration {:site i :index 9}}))))
          (is (= "unknown-iteration" (:error (ask! {:op :debug-recorded :unit lp :iteration {:site twice :index 1}})))))
        (testing "a cursor at a form, in a loop, moved round it"
          (let [{:keys [frames] :as cur} (ask! {:op :debug-cursor :unit lp :site twice})
                [l f] frames]
            (is (= {:kind "loop" :label "loop i acc" :loop i :index 3 :of 4 :code "(* i 2)" :level 0
                    :line 4 :column 29 :site-kind "value"}
                   (select-keys l [:kind :label :loop :index :of :code :level :line :column :site-kind])))
            (is (= {:kind "call" :label "lp" :index 1 :of 1 :iteration {:site i :index 3} :code "(* i 2)"}
                   (select-keys f [:kind :label :index :of :iteration :code])))
            (let [move (fn [cur m & more]
                         (ask! (merge {:op :debug-cursor :unit lp :call (:call cur) :at (:at cur) :move m}
                                      (apply hash-map more))))
                  back (move cur "previous-iteration")
                  after (move back "next")
                  last-one (move back "iteration" :index 4)]
              (is (= [2 "(* i 2)"] ((juxt :index :code) (first (:frames back)))))
              (is (= "(+ acc (* i 2))" (:code (first (:frames after)))))
              (is (= "(* i 2) was not evaluated in iteration 4" (:note last-one)))
              (is (= [4 "i"] ((juxt :index :code) (first (:frames last-one)))) "at its first value")
              (is (= "no-move" (:error (move cur "previous-call"))) "the first call kept"))))
        (testing "the first iteration where code is true, with the locals of a form bound"
          (let [find! #(ask! (merge {:op :debug-find-iteration :unit lp :site i :line 4 :column 29} %))]
            (is (= {:index 3} (select-keys (find! {:code "(= i 2)"}) [:index :error])))
            (is (= {:index 4} (select-keys (find! {:code "(< 5 acc)"}) [:index :error])))
            (is (= {:index 1} (select-keys (find! {:code "(= n 3)"}) [:index :error])) "a param, further out")
            (is (= {:index 2} (select-keys (find! {:code "(< i 2)" :backward true :from 4}) [:index :error])))
            (is (= {:index 4} (select-keys (find! {:code "(= n 3)" :backward true :line 2 :column 10})
                                           [:index :error])))
            (is (= {:index 3} (select-keys (find! {:code "(= acc 2)" :line 2 :column 10}) [:index :error]))
                "where i is bound: the names the loop binds each time round")
            (is (= {} (select-keys (find! {:code "(= i 2)" :from 3}) [:index :error])) "none after it")
            (is (= {:error "boom"}
                   (select-keys (find! {:code "(if (= i 1) (throw (ex-info \"boom\" 1)) false)"}) [:index :error]))
                "what the code threw")
            (is (= {:index 2} (select-keys (find! {:code "(if (= i 0) (throw (ex-info \"boom\" 1)) (= i 1))"})
                                           [:index :error]))
                "true in one: what it threw before is not said")
            (is (= "compilation" (:error (find! {:code "(= nope 2)"}))))
            (is (= "unknown-iteration" (:error (find! {:code "true" :site twice})))))))
      (testing "the calls a call made, and the cursor going into one"
        (eval-text r (src 10 "(defn inner [x] (* x 3))\n"))
        (let [inner (unit!)]
          (eval-text r (src 20 "(defn outer [y]\n  (+ (inner y) (inner (inc y))))\n"))
          (let [outer (unit!)]
            (is (= ["9" "15"] (rets (eval-text r "(outer 1)\n(outer 2)\n" 2))))
            (let [roots (filter #(= outer (:unit %)) (:calls (ask! {:op :debug-calls})))
                  [o1 o2] roots
                  made (:calls (ask! {:op :debug-calls :unit outer :call (:call o2)}))]
              (is (= [{:label "outer" :index 1 :of 2 :args ["1"] :value "9" :made 2}
                      {:label "outer" :index 2 :of 2 :args ["2"] :value "15" :made 2}]
                     (map #(select-keys % [:label :index :of :args :value :made]) roots)))
              (is (= [[inner ["2"] "6"] [inner ["3"] "9"]] (map (juxt :unit :args :value) made)))
              (is (not-any? :made-in-gone made))
              (let [at-call (ask! {:op :debug-cursor :unit outer :site (:site (at-place (:values (ask! {:op :debug-recorded :unit outer})) 21 6))})
                    in (ask! {:op :debug-cursor :unit outer :call (:call at-call) :at (:at at-call) :move "in"})
                    [f0 f1] (:frames in)]
                (is (= [inner "call" "[x]" 0] ((juxt :unit :kind :code :level) f0)) "at what it was called with")
                (is (= [outer "call" "(inner y)" 1] ((juxt :unit :kind :code :level) f1)))
                (is (= (:call o2) (:call f1)))
                (let [out (ask! {:op :debug-cursor :unit inner :call (:call in) :at (:at in) :move "out"})]
                  (is (= [outer "(inner y)"] ((juxt :unit :code) (first (:frames out))))))
                (testing "from the frame further out, into the same call of the other one"
                  (let [{:keys [frames frame]} (ask! {:op :debug-cursor :unit inner :call (:call in) :at (:at in)
                                                      :frame 1 :move "previous-call"})]
                    (is (= 1 frame) "the frame moved from")
                    (is (= [inner "[x]"] ((juxt :unit :code) (first frames))))
                    (is (= [(:call o1) 1] ((juxt :call :index) (second frames)))))))
              (testing "of the calls made for the same value, the first ones and the last ones"
                (eval-text r (src 30 (str "(defn many [k]\n  (loop [i 0 acc 0]\n"
                                          "    (if (< i k) (recur (inc i) (+ acc (inner i))) acc)))\n")))
                (let [many (unit!)]
                  (is (= ["1305"] (rets (eval-text r "(many 30)\n"))))
                  (let [[m] (filter #(= many (:unit %)) (:calls (ask! {:op :debug-calls})))]
                    (is (= 20 (:made m)))
                    (let [[l] (:loops (ask! {:op :debug-recorded :unit many}))]
                      (is (= [[[1 9] [22 31]] 31] ((juxt :kept :iterations) l)) "the first 9 and the last 10")
                      (is (= "unknown-iteration"
                             (:error (ask! {:op :debug-recorded :unit many :iteration {:site (:site l) :index 15}}))))
                      (is (= "9" (:value (at-place (:values (ask! {:op :debug-recorded :unit many
                                                                   :iteration {:site (:site l) :index 4}}))
                                                   32 39)))
                          "(inner i) in the fourth"))
                    (is (= (concat (range 10) (range 20 30))
                           (map #(parse-long (first (:args %)))
                                (:calls (ask! {:op :debug-calls :unit many :call (:call m)}))))))))))))
      (testing "a fn the source wrote in another goes round once a call of it"
        (eval-text r (src 40 "(defn tw [a]\n  (let [f (fn [x] (* x 10))]\n    (+ (f a) (f 2))))\n"))
        (let [tw (unit!)]
          (is (= ["30"] (rets (eval-text r "(tw 1)\n"))))
          (let [{:keys [loops values]} (ask! {:op :debug-recorded :unit tw})
                e (:site (at-place values 41 11))]
            (is (= [{:site e :fn true :iterations 2 :kept [[1 2]] :line 41 :column 11 :end-line 41 :end-column 28}]
                   loops))
            (is (= ["10" "20"]
                   (for [k [1 2]]
                     (:value (at-place (:values (ask! {:op :debug-recorded :unit tw :iteration {:site e :index k}}))
                                       41 19)))))))))
    (ask! {:op :debug-uninstrument})
    (close r)
    (close c)))

(deftest going-through-on-the-jvm
  (going-through {}))

(deftest going-through-in-js
  (if-not @node?
    (println "node isn't installed: skipping going through JS recordings")
    (going-through {:target :js})))

(defn- calls-only
  "Which functions are recorded, and the calls only of a library's, on the
  target of a repl opened with `hello`, whose dialect keys are `keys`: the
  same answers on both."
  [hello keys]
  (let [r (repl hello)
        c (control)
        ask! (let [id (atom 200)] #(ask c (merge keys (assoc % :id (swap! id inc)))))
        dir (:directory (:description *server*))
        app (io/file dir "src" "calls" "app.rpq")
        at (fn [file line record?]
             (str "#replique/src {:file " (pr-str (str file)) " :line " line " :column 1"
                  (when record? " :instrument true :entry false :record true") "}\n"))
        project (str "(defn helper [y] (calls.lib/twice y))\n"
                     "(defn top [n] (+ (helper n) (calls.lib/twice n) (calls.lib/many)))\n"
                     "(defn caller [] (top 1))\n"
                     "(defn tries [] (try (calls.lib/boom 1) (catch :default e :caught)))\n")
        lines (str/split-lines project)]
    (io/make-parents app)
    (spit app (str "(ns calls.core)\n" project))
    ;; a library: defined at a prompt, or in a file elsewhere
    (eval-text r (str "#replique/ns calls.lib\n\n"
                      "(defn twice [x] (* 2 x))\n(defn boom [x] (throw (ex-info \"no\" 1)))\n"
                      "(defn many ([] 0) ([x] x))\n(def k 3)\n(defmacro mm [] 1)\n")
               6)
    (eval-text r "#replique/ns calls.core\n\n")
    (doseq [[i line] (map-indexed vector lines)]
      (eval-text r (str (at app (+ 2 i) false) line "\n")))
    (testing "the functions of the project around one, as the compiler saw them"
      (is (= {:vars [{:var "calls.core/top" :file (str app)}
                     {:var "calls.core/caller" :file (str app)}
                     {:var "calls.core/helper" :file (str app)}]
              :callers ["calls.core/caller"] :callees ["calls.core/helper"]}
             (select-keys (ask! {:op :recording-scope :around "calls.core/top"}) [:vars :callers :callees])))
      (is (= ["calls.core/caller" "calls.core/top"]
             (:callers (ask! {:op :recording-scope :around "calls.core/helper" :depth 2}))))
      (is (= "unknown-var" (:error (ask! {:op :recording-scope :around "calls.core/nope"}))))
      (is (= "invalid-message" (:error (ask! {:op :recording-scope :around "top"})))))
    (eval-text r (str (at app 3 true) (nth lines 1) "\n"))
    (eval-text r (str (at app 5 true) (nth lines 3) "\n"))
    (let [[top tries] (map :unit (repeatedly 2 #(event c "debug-instrumented")))
          {:keys [recorded skipped events]}
          (ask! {:op :record-calls :callees-of ["calls.core/top" "calls.core/tries"]
                 :vars ["calls.lib/k" "calls.lib/mm" "replique.core/prn" "calls.lib/nope" "calls.core/top"]})
          events (filter #(= "debug-instrumented" (:event %)) events)]
      (testing "the library functions code recorded calls, and why not the others"
        (is (= ["calls.lib/boom" "calls.lib/many" "calls.lib/twice"] recorded)
            "not the project's helper")
        (is (= [{:var "calls.lib/k" :why "not a function"}
                {:var "calls.lib/mm" :why "a macro"}
                {:var "replique.core/prn" :why "called everywhere, or what recording is made of"}
                {:var "calls.lib/nope" :why "no such var"}
                {:var "calls.core/top" :why "its code is recorded"}]
               skipped))
        (is (= 3 (count events)))
        (is (every? #(and (:calls-only %) (= "calls.lib" (:ns %))) events) "told, as calls only")
        (let [again (ask! {:op :record-calls :vars ["calls.lib/twice"]})]
          (is (= {:recorded ["calls.lib/twice"] :skipped []} (select-keys again [:recorded :skipped])))
          (is (not-any? #(= "debug-instrumented" (:event %)) (:events again)) "already")))
      (let [[boom many twice] (map :unit (sort-by :unit events))
            _ (is (= ["8"] (rets (eval-text r "(top 2)\n"))))
            _ (is (= ["10"] (rets (eval-text r "(calls.lib/twice 5)\n"))))
            [call] (filter #(= top (:unit %)) (:calls (ask! {:op :debug-calls})))
            made (:calls (ask! {:op :debug-calls :unit top :call (:call call)}))]
        (testing "a call made in a call recorded is one of its calls; one made in none, only made"
          (is (= [["calls.lib/twice" ["2"] "4"] ["calls.lib/twice" ["2"] "4"] ["calls.lib/many" nil "0"]]
                 (map (juxt :label :args :value) made))
              "the one helper made too, while top ran")
          (is (= [twice twice many] (map :unit made)))
          (is (= 2 (count (:calls (ask! {:op :debug-recorded :unit twice}))))))
        (testing "going into one: what it was called with, then what it returned"
          (let [{:keys [frames] :as cur} (ask! {:op :debug-cursor :unit twice})
                after (ask! {:op :debug-cursor :unit twice :call (:call cur) :at (:at cur) :move "next"})]
            (is (= ["call" "calls.lib/twice" "[x]"] ((juxt :kind :label :code) (first frames))))
            (is (= [top "call"] ((juxt :unit :kind) (second frames))) "in the call that made it")
            (is (= "returned" (:code (first (:frames after)))))))
        (testing "what it threw"
          (is (= [":caught"] (rets (eval-text r "(tries)\n"))))
          (let [[call] (filter #(= tries (:unit %)) (:calls (ask! {:op :debug-calls})))
                [b] (:calls (ask! {:op :debug-calls :unit tries :call (:call call)}))]
            (is (= [boom ["1"] true] ((juxt :unit :args :threw) b)))
            (is (str/ends-with? (:value b) "ExceptionInfo: no") "by its class and message")))
        (testing "defined again, wrapped again"
          (eval-text r "#replique/ns calls.lib\n(defn twice [x] (* 3 x))\n#replique/ns calls.core\n\n" 2)
          (is (= ["12"] (rets (eval-text r "(top 2)\n"))))
          (let [call (last (filter #(= top (:unit %)) (:calls (ask! {:op :debug-calls}))))]
            (is (= "6" (:value (first (:calls (ask! {:op :debug-calls :unit top :call (:call call)}))))))))
        (testing "its code recorded, its calls only are not any more"
          (eval-text r "#replique/ns calls.lib\n\n")
          (eval-text r (str (at "/elsewhere/lib.rpq" 2 true) "(defn many ([] 0) ([x] x))\n"))
          (eval-text r "#replique/ns calls.core\n\n")
          (is (= many (:unit (event c "debug-uninstrumented"))))
          (let [code (:unit (event c "debug-instrumented"))]
            (testing "with the last code recorded in its runtime, they go"
              (is (= [top] (:uninstrumented (ask! {:op :debug-uninstrument :unit top}))))
              (is (= [code] (:uninstrumented (ask! {:op :debug-uninstrument :unit code}))))
              (is (= (sort [boom twice tries]) (:uninstrumented (ask! {:op :debug-uninstrument :unit tries}))))
              (is (= ["6"] (rets (eval-text r "(calls.lib/twice 2)\n"))) "what it held again"))))))
    (ask! {:op :debug-uninstrument})
    (close r)
    (close c)))

(deftest calls-only-on-the-jvm
  (calls-only {} {}))

(deftest calls-only-in-js
  (if-not @node?
    (println "node isn't installed: skipping calls only in JS")
    (calls-only {:target :js} {:dialect :cljs :target :node})))

(deftest a-class-is-not-wrapped-in-js
  (if-not @node?
    (println "node isn't installed: skipping a class in JS")
    (let [r (repl {:target :js})
          c (control)
          keys {:dialect :cljs :target :node}]
      (eval-text r "#replique/ns cls.lib\n\n(def M js/Map)\n(defn f [] (new M))\n" 3)
      (is (= {:recorded ["cls.lib/f"] :skipped [{:var "cls.lib/M" :why "a class, made with new"}]}
             (select-keys (ask c (merge keys {:op :record-calls :id 2 :vars ["cls.lib/M" "cls.lib/f"]}))
                          [:recorded :skipped])))
      (is (= ["true"] (rets (eval-text r "(instance? js/Map (f))\n"))) "made with new still")
      (ask c (merge keys {:op :debug-uninstrument :id 3}))
      (close r)
      (close c))))

(defn- inspecting
  "The views of the inspector, on the target of a repl opened with `hello`:
  the same answers on both, but what is the target's own."
  [hello]
  (let [r (repl hello)
        c (control)
        js? (= :js (:target hello))
        dialect (when js? {:dialect :cljs :target :node})
        unit! #(:unit (event c "debug-instrumented"))
        ask! (let [id (atom 100)] #(ask c (assoc % :id (swap! id inc))))
        open! #(ask! (merge {:op :inspect :source %} dialect))
        keys-of (fn [page] (mapv (juxt :key :value) (:children page)))]
    (eval-text r "#replique/ns insp.core\n\n")
    (eval-text r (src 1 "(defn rf [m]\n  (let [x (inc m)]\n    (* x 2)))\n"))
    (let [rf (unit!)]
      (is (= ["4" "12"] (rets (eval-text r "(rf 1)\n(rf 5)\n" 2))))
      (let [{:keys [values]} (ask! {:op :debug-recorded :unit rf})
            times (:site (at-place values 3 5))
            entry (:site (first (filter #(= "enter" (:kind %)) values)))
            cur (ask! {:op :debug-cursor :unit rf :site times})]
        (testing "what a cursor is at: the value under its code, and the locals"
          (let [v (open! {:debug {:cursor (select-keys cur [:unit :call :at])}})]
            (is (= {:value "(* x 2) 12 · locals m 5  x 6" :kind "outline" :expandable true}
                   (select-keys (:root v) [:value :kind :expandable])))
            (is (= [["(* x 2)" "12"] ["locals" "m 5  x 6"]] (keys-of v)))
            (is (:above (first (:children v))) "code is on a line of its own")
            (is (= [["m" "5"] ["x" "6"]]
                   (keys-of (ask! {:op :inspect-children :view (:view v) :node (:node (second (:children v)))}))))
            (is (= {:value "(* x 2) 12 · locals m 5  x 6" :whole true}
                   (select-keys (ask! {:op :inspect-print :view (:view v) :node 0}) [:value :whole])))
            (if js?
              (is (= "invalid-message" (:error (ask! {:op :inspect-def :view (:view v) :node 1 :name "kept"}))))
              (do (is (= "insp.core/kept" (:var (ask! {:op :inspect-def :view (:view v) :node 1 :name "kept"}))))
                  (is (= ["12"] (rets (eval-text r "kept\n"))) "kept to work with at the repl")
                  (is (= "invalid-message" (:error (ask! {:op :inspect-def :view (:view v) :node 2 :name "kept"})))
                      "an outline is not a value")))
            (is (= {:closed true} (select-keys (ask! {:op :inspect-close :view (:view v)}) [:closed])))
            (is (= "unknown-view" (:error (ask! {:op :inspect-refresh :view (:view v)}))))))
        (testing "what a form evaluated to in each call, and refreshed"
          (let [v (open! {:debug {:site times}})]
            (is (= "call 1 4  call 2 12" (:value (:root v))))
            (eval-text r "(rf 7)\n")
            (is (= [["call 1" "4"] ["call 2" "12"] ["call 3" "16"]]
                   (keys-of (ask! {:op :inspect-refresh :view (:view v)}))))))
        (testing "what a fn was called with"
          (is (= "5" (:value (:root (open! {:debug {:site entry :call (:call cur) :arg 0}}))))))
        (testing "each call, by its params"
          (is (= "call 1 m 1  (* x 2) 4 · call 2 m 5  (* x 2) 12 · call 3 m 7  (* x 2) 16"
                 (:value (:root (open! {:debug {:calls rf :site times}}))))))))
    (eval-text r (src 10 (str "(defn lp [n]\n  (loop [i 0 acc 0]\n    (if (< i n)\n"
                              "      (let [d (* i 2)] (recur (inc i) (+ acc d)))\n      acc)))\n")))
    (let [lp (unit!)]
      (is (= ["6"] (rets (eval-text r "(lp 3)\n"))))
      (let [{:keys [loops values]} (ask! {:op :debug-recorded :unit lp})
            i (:site (first loops))
            twice (:site (at-place values 13 15))]
        (testing "each iteration of a loop, by the names bound in it"
          (is (= [["iteration 1" "i 0  acc 0  d 0"] ["iteration 2" "i 1  acc 0  d 2"]
                  ["iteration 3" "i 2  acc 2  d 4"] ["iteration 4" "i 3  acc 6"]]
                 (keys-of (open! {:debug {:loop i}}))))
          (is (= [["iteration 1" "i 0  acc 0  d 0  (* i 2) 0"]]
                 (take 1 (keys-of (open! {:debug {:loop i :site twice}})))))
          (is (= "iteration 1 0  iteration 2 2  iteration 3 4"
                 (:value (:root (open! {:debug {:loop i :site twice :only true}})))))
          (is (= "2" (:value (:root (open! {:debug {:site twice :iteration {:site i :index 2}}})))))
          (is (= ":not-recorded" (:value (:root (open! {:debug {:site twice :iteration {:site i :index 4}}}))))
              "not evaluated in the last one")
          (is (= "unknown-iteration" (:error (open! {:debug {:loop twice}})))))))
    (testing "a var, and an exception, its message and data"
      (eval-text r "(def v 42)\n(def e (ex-info \"boom\" 1))\n" 2)
      (is (= {:value "42" :kind "number"} (select-keys (:root (open! {:var "insp.core/v"})) [:value :kind])))
      (let [v (open! {:var "insp.core/e"})]
        (is (= ["object" "ExceptionInfo" true] ((juxt :kind :type :expandable) (:root v))))
        (is (= ["#object[ExceptionInfo] {" [["message" "\"boom\""] ["data" "1"]]] [(:open v) (keys-of v)])))
      (is (= "unknown-var" (:error (open! {:var "insp.core/nope"}))))
      (is (= "invalid-message" (:error (open! {:results "c1"}))) "Replique keeps no *1"))
    (when js?
      (testing "a JS object, and an array, sent a part at a time"
        (eval-text r (str "(def o #js {:a 1 :b #js [1 #js {:c \"x\"}]})\n"
                          "(def big #js [" (str/join " " (range 250)) "])\n")
                   2)
        (let [v (ask! {:op :inspect :source {:var "insp.core/o"} :dialect :cljs :expand 20})
              [_ b] (:children v)]
          (is (= ["#js {" [["a" "1"] ["b" "#array[1 #object[Object]]"]]] [(:open v) (keys-of v)]))
          (is (= [["0" "1"] ["1" "#object[Object]"]] (keys-of (:page b))) "opened, holding what it does not show")
          (is (= [["c" "\"x\""]] (keys-of (:page (second (:children (:page b))))))))
        (let [v (ask! {:op :inspect :source {:var "insp.core/big"} :dialect :cljs :limit 5})]
          (is (= ["#array[0 1 2 3 4 5 6 7 ...]" 250] ((juxt :value :count) (:root v))))
          (is (= [["200" "200"] ["201" "201"]]
                 (keys-of (ask! {:op :inspect-children :view (:view v) :node 0 :offset 200 :limit 2})))
              "past what was sent first")
          (is (= 250 (count (str/split (str/replace (:value (ask! {:op :inspect-print :view (:view v) :node 0}))
                                                    #"#array\[|\]" "")
                                       #" ")))))))
    (testing "a view goes with the connection that opened it"
      (let [other (control)
            v (do (send-text other (str (pr-str (merge {:op :inspect :source {:var "insp.core/v"} :id 1} dialect)) "\n"))
                  (frame other))]
        (close other)
        (Thread/sleep 100)
        (is (= "unknown-view" (:error (ask! {:op :inspect-refresh :view (:view v)}))))))
    (ask! {:op :debug-uninstrument})
    (close r)
    (close c)))

(deftest inspecting-on-the-jvm
  (inspecting {}))

(deftest inspecting-in-js
  (if-not @node?
    (println "node isn't installed: skipping the inspector in JS")
    (inspecting {:target :js})))

(deftest repls-share-their-target
  (let [a (repl {})
        b (repl {:dialect :clj})]
    (eval-text a "#replique/ns shared.core\n(defn k [] :a)\n")
    (is (= [":a"] (rets (eval-text b "(shared.core/k)\n"))))
    (eval-text a "(defn k [] :b)\n")
    (is (= [":b"] (rets (eval-text b "(shared.core/k)\n"))))
    (close a)
    (close b)))

(deftest interrupt
  (let [r (repl {})
        c (connect {:op :hello :role :control :id 1})
        _ (frame c)
        id (:connection (:hello r))]
    (send-text c (str (pr-str {:op :interrupt :connection id :id 2}) "\n"))
    (is (= false (:interrupted (frame c))) "an idle repl is left alone")
    (send-text r "(Thread/sleep 20000)\n")
    (Thread/sleep 300)
    (send-text c (str (pr-str {:op :interrupt :connection id :id 3}) "\n"))
    (is (= true (:interrupted (frame c))))
    (is (= "execution" (:phase (first (until-prompt r)))))
    (is (= ["1"] (rets (eval-text r "1\n"))))
    (close r)
    (close c)))

(deftest js-repl
  (if-not @node?
    (println "node isn't installed: skipping the JS repl")
    (let [c (redefinition {:target :js})]
      (is (= "js" (:target (:hello c))))
      (testing "output, and failures"
        (is (= [{:tag "out" :string "a 1\n"} {:tag "ret" :ns "user" :value "nil"}]
               (butlast (eval-text c "(println \"a\" 1)\n"))))
        (let [[e] (eval-text c "(throw (ex-info \"boom\" 7))\n")]
          (is (= ["execution" "ExceptionInfo" "7"]
                 ((juxt :phase (comp :class :exception) (comp :data :exception)) e))))
        (is (= ["0.5"] (rets (eval-text c "(/ 1 2)\n")))))
      (testing "replique.el's spelling of a node repl is a JS repl"
        (let [d (repl {:dialect :cljs :target :node})]
          (is (= "js" (:target (:hello d))))
          (is (= ["2"] (rets (eval-text d "(user/f)\n"))) "and shares the namespaces")
          (close d)))
      (close c))))

(deftest shutdown
  (let [dir (str (temp-dir))
        s (server/start {:process-id "going" :directory dir})
        c (binding [*server* s] (connect {:op :hello :role :control :id 1}))]
    (frame c)
    (send-text c "{:op :shutdown :id 2}\n")
    (is (= true (:stopping (frame c))))
    (is (nil? (frame c)) "the connection closing says the server went")
    (is (deref (:stopped s) 5000 false))
    (is (not (.exists (io/file dir ".replique" "processes" "going.json"))))))
