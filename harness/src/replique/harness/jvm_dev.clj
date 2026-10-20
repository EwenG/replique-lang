(ns replique.harness.jvm-dev
  "The JVM dev cell (doc/conformance.md §4): Replique's JVM back end in dev
  mode, in the harness's own JVM, through the path the REPL takes
  (replique.jvm.dev): each form analysed, compiled to hidden classes and run.

  Each file runs in a fresh namespace and a fresh compiler environment. A
  result is printed by the kernel's printer and read back, so what the
  runner compares is what a Replique program would print.

  `(cell {:record? true})` compiles every form to be recorded, every site
  chosen, locals included, reporting to a hook that only hands values back:
  recording must not change what a program computes (doc/recording.md §5)."
  (:require [replique.analyzer :as a]
            [replique.harness.canon :as canon]
            [replique.harness.cell :as cell]
            [replique.jvm.dev :as dev])
  (:import [replique.lang ExceptionInfo Hook Namespace Printer]))

(def ^:private record
  "Every site, and a hook that changes nothing."
  {:site (let [n (atom 0)] (fn [_] (swap! n inc)))
   :locals? true
   :hook (reify Hook
           (enter [_ _ _ _] nil)
           (value [_ _ v _] v)
           (exit [_ _]))})

(defn- data [x]
  (canon/read-printed (Printer/print x) :jvm))

(defn- thrown [^Throwable t]
  {:result :thrown
   :category (cell/category (.getName (class t)) (.getMessage t))
   :data (when (instance? ExceptionInfo t) (data (.-data ^ExceptionInfo t)))
   :message (.getMessage t)})

(defn- evaluate [rt cenv ns form record?]
  (try
    (let [{:keys [error value]} (dev/eval-form rt cenv form (cond-> {:ns ns} record? (assoc :record record)))]
      (if error
        {:result :compile-error :code (:code error) :message (:message error)}
        {:result :value :value (data value)}))
    (catch Throwable t
      (thrown t))))

(defn- run-case [rt cenv ns c record?]
  (loop [steps (:steps c) out []]
    (if-let [s (first steps)]
      (let [r (evaluate rt cenv ns (if (= :form (:op s)) (:form s) (:expr s)) record?)
            out (conj out r)]
        (if (and (= :form (:op s)) (not= :value (:result r)))
          out
          (recur (rest steps) out)))
      out)))

(deftype JvmDevCell [rt record?]
  cell/Cell
  (-info [_] (cond-> {:id :jvm-dev :target :jvm :mode :dev}
              record? (assoc :id :jvm-dev-recorded :recorded? true)))
  (-run-file [_ file cases]
    (let [ns (:ns file)
          cenv (a/environment)]
      (Namespace/remove (str ns))
      (let [r (evaluate rt cenv 'user (:ns-form file) false)]
        (when-not (= :value (:result r))
          (throw (ex-info (str "the ns form failed: " (or (:message r) (:code r))) r))))
      (into {} (map (fn [c] [(:id c) (run-case rt cenv ns c record?)])) cases)))
  (-close [_]))

(defn cell
  ([] (cell {}))
  ([{:keys [record?]}] (->JvmDevCell (dev/runtime) (boolean record?))))
