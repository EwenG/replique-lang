(ns replique.harness.oracle.clojure
  "The Clojure oracle cell (doc/conformance.md §6): ^:clojure cases run on
  replique-clj, in this JVM, to check the expectations themselves.

  It plays a JVM dev cell: one fresh namespace per file, each form evaluated
  as Clojure's REPL would evaluate it."
  (:require [replique.harness.cell :as cell])
  (:import [clojure.lang Compiler$CompilerException]))

(defn- thrown [^Throwable t]
  {:result :thrown
   :category (cell/category (.getName (class t)) (.getMessage t))
   :data (ex-data t)
   :message (.getMessage t)})

(defn- evaluate
  "Evaluate `form` in `ns` and return a step result.

  Clojure throws a run-time exception from eval as it is, and wraps a compile
  error in a CompilerException; an exception raised while running a top-level
  initialiser comes wrapped with the :execution phase, and is unwrapped."
  [ns form]
  (try
    {:result :value
     :value (binding [*ns* ns] (eval form))}
    (catch Compiler$CompilerException e
      (if (and (= :execution (:clojure.error/phase (ex-data e))) (.getCause e))
        (thrown (.getCause e))
        {:result :compile-error
         :code :clojure/compile
         :message (.getMessage (or (.getCause e) e))}))
    (catch Throwable t
      (thrown t))))

(defn- run-case [ns c]
  (loop [steps (:steps c) out []]
    (if-let [s (first steps)]
      (let [r (evaluate ns (if (= :form (:op s)) (:form s) (:expr s)))
            out (conj out r)]
        (if (and (= :form (:op s)) (not= :value (:result r)))
          out
          (recur (rest steps) out)))
      out)))

(defn- fresh-ns
  "The file's namespace, created from scratch: a run must not see what an
  earlier run defined."
  [{:keys [ns ns-form]}]
  (remove-ns ns)
  (binding [*ns* (the-ns 'user)]
    (eval ns-form))
  (the-ns ns))

(deftype ClojureCell []
  cell/Cell
  (-info [_] {:id :oracle-clojure :target :jvm :mode :dev :oracle? true})
  (-run-file [_ file cases]
    (let [ns (fresh-ns file)]
      (try
        (into {} (map (fn [c] [(:id c) (run-case ns c)])) cases)
        (finally (remove-ns (:ns file))))))
  (-close [_]))

(defn cell [] (->ClojureCell))
