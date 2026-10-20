(ns replique.harness.oracle.cljs
  "The ClojureScript oracle cell (doc/conformance.md §6): ^:clojure cases run on
  replique-cljs, in a Node runtime, to check the expectations themselves.

  It plays a JS dev cell. cljs.core is compiled once, into the directory the
  runtime loads from, and every file then gets a namespace of its own in the
  same compile environment, as replique-cljs's own REPL tests do.

  Needs replique-cljs on the classpath (the :cljs alias) and node on the PATH.

  Each check runs as a probe that catches what the expression throws, so a
  thrown exception comes back as data rather than as REPL text:

    [:value v]  or  [:thrown class-name message data]

  printed by cljs.core and read back here."
  (:require [clojure.cljs.driver :as driver]
            [clojure.cljs.env :as env]
            [clojure.cljs.repl :as repl]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [replique.harness.canon :as canon]
            [replique.harness.cell :as cell])
  (:import [java.io File StringWriter]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- probe
  "`expr` wrapped so its value or its exception comes back as data. Built by
  hand rather than syntax-quoted, so every symbol is the cljs.core one."
  [expr]
  (list 'try
        [:value expr]
        (list 'catch :default 'e
              [:thrown
               (list 'if (list 'cljs.core/instance? 'cljs.core/ExceptionInfo 'e)
                     "cljs.core/ExceptionInfo"
                     (list 'cljs.core/str (list 'cljs.core/type 'e)))
               (list 'cljs.core/ex-message 'e)
               (list 'cljs.core/ex-data 'e)])))

(defn- read-back
  "A value cljs.core printed, as JVM data. Unknown tags (#object, #js) are kept
  as tagged literals. Text that isn't data (a var prints as #'ns/x) is kept as
  a string tagged #unreadable."
  [s]
  (try
    (canon/js-value (edn/read-string {:default tagged-literal} s))
    (catch Exception _
      (tagged-literal 'unreadable s))))

(defn- step-result [{:keys [status phase value]} probe?]
  (cond
    (and (= :success status) (not probe?))
    {:result :value :value (read-back value)}

    (= :success status)
    (let [[tag a b c] (read-back value)]
      (case tag
        :value {:result :value :value a}
        :thrown {:result :thrown :category (cell/category a b) :message b :data c}
        {:result :thrown :category :host :message (str "unexpected probe result " value)}))

    (= :compile phase)
    {:result :compile-error :code :cljs/compile :message value}

    :else
    {:result :thrown :category :host :message (str value)}))

(defn- run-case [cenv rt opts c]
  (loop [steps (:steps c) out []]
    (if-let [s (first steps)]
      (let [form? (= :form (:op s))
            r (step-result (repl/eval-form cenv rt (if form? (:form s) (probe (:expr s))) opts)
                           (not form?))
            out (conj out r)]
        (if (and form? (not= :value (:result r)))
          out
          (recur (rest steps) out)))
      out)))

(defn- delete-tree! [^File f]
  (when (.isDirectory f)
    (run! delete-tree! (.listFiles f)))
  (.delete f))

(deftype CljsCell [cenv rt ^File dir program-out]
  cell/Cell
  (-info [_] {:id :oracle-cljs :target :js :mode :dev :oracle? true})
  (-run-file [_ file cases]
    (env/with-current-ns 'cljs.user
      (let [opts {:out-dir dir}
            ns-result (repl/eval-form cenv rt (:ns-form file) opts)]
        (when-not (= :success (:status ns-result))
          (throw (ex-info (str "the ns form failed: " (:value ns-result)) {})))
        (into {} (map (fn [c] [(:id c) (run-case cenv rt opts c)])) cases))))
  (-close [_]
    (.close ^java.io.Closeable rt)
    (delete-tree! dir)))

(defn cell
  "Compile cljs.core and start a Node runtime. Takes a while: cljs.core is
  analysed from source."
  []
  (let [dir (.toFile (Files/createTempDirectory "replique-conformance-cljs"
                                                (make-array FileAttribute 0)))
        program-out (StringWriter.)]
    (env/with-current-ns 'cljs.user
      (let [cenv (env/compile-env {:core-macros 'cljs.core})]
        (driver/compile-namespace! cenv 'cljs.core {:out-dir dir})
        (->CljsCell cenv
                    (repl/node-runtime {:dir dir :out program-out})
                    dir
                    program-out)))))
