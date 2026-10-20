(ns replique.harness.js-dev
  "The JS dev cell (doc/conformance.md §4): Replique's JS back end in dev
  mode, in one long-lived Node process, through the path the REPL takes
  (replique.js.dev): each form analysed for JS, compiled and run in Node.

  Each file runs in a fresh namespace, in Node and in a fresh compiler
  environment. A result is printed by the kernel's printer in Node and read
  back here.

  `(cell {:record? true})` compiles every form to be recorded, every site
  chosen, locals included, reporting to a hook that only hands values back:
  recording must not change what a program computes (doc/recording.md §5).

  Needs node on the PATH."
  (:require [replique.analyzer :as a]
            [replique.harness.canon :as canon]
            [replique.harness.cell :as cell]
            [replique.js.dev :as dev]))

(defn- data [printed]
  (canon/read-printed printed :js))

(def ^:private record
  "Every site, and a hook that changes nothing."
  {:site (let [n (atom 0)] (fn [_] (swap! n inc)))
   :locals? true
   :hook "({enter(n, o, a) { return null; }, value(n, v, c) { return v; }, exit(c) {}})"})

(defn- evaluate [rt cenv ns form record?]
  (let [{:keys [error value thrown]} (dev/eval-form rt cenv form (cond-> {:ns ns} record? (assoc :record record)))]
    (cond
      error {:result :compile-error :code (:code error) :message (:message error)}
      thrown {:result :thrown
              :category (cell/category (:class thrown) (:message thrown))
              :data (data (:data thrown))
              :message (:message thrown)}
      :else {:result :value :value (data value)})))

(defn- run-case [rt cenv ns c record?]
  (loop [steps (:steps c) out []]
    (if-let [s (first steps)]
      (let [r (evaluate rt cenv ns (if (= :form (:op s)) (:form s) (:expr s)) record?)
            out (conj out r)]
        (if (and (= :form (:op s)) (not= :value (:result r)))
          out
          (recur (rest steps) out)))
      out)))

(deftype JsDevCell [rt record?]
  cell/Cell
  (-info [_] (cond-> {:id :js-dev :target :js :mode :dev}
              record? (assoc :id :js-dev-recorded :recorded? true)))
  (-run-file [_ file cases]
    (let [ns (:ns file)
          cenv (a/environment)]
      (dev/remove-ns! rt ns)
      (let [r (evaluate rt cenv 'user (:ns-form file) false)]
        (when-not (= :value (:result r))
          (throw (ex-info (str "the ns form failed: " (or (:message r) (:code r))) r))))
      (into {} (map (fn [c] [(:id c) (run-case rt cenv ns c record?)])) cases)))
  (-close [_] (dev/close rt)))

(defn cell
  ([] (cell {}))
  ([{:keys [record?]}] (->JsDevCell (dev/runtime) (boolean record?))))
