(ns replique.harness.main
  "The command line:

    clojure -M:conformance [--cells a,b] [--out file] [path ...]
    clojure -M:cljs:conformance --cells oracle-clojure,oracle-cljs

  Paths are case files or directories, default conformance/. Cells default to
  every cell that can be built on this classpath. Exits with 1 when a check
  fails, so CI can use it."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [replique.harness.cell :as cell]
            [replique.harness.report :as report]
            [replique.harness.runner :as runner]))

(def ^:private known-cells
  "Each cell's constructor, resolved only when the cell is asked for, so the
  ClojureScript oracle loads only when replique-cljs is on the classpath. The
  other Replique cells join this table as the back ends are built (M1)."
  {:oracle-clojure 'replique.harness.oracle.clojure/cell
   :oracle-cljs 'replique.harness.oracle.cljs/cell
   :jvm-dev 'replique.harness.jvm-dev/cell
   :js-dev 'replique.harness.js-dev/cell})

(defn- available? [ctor]
  (try (some? (requiring-resolve ctor))
       (catch Throwable _ false)))

(defn- default-cells []
  (filterv #(available? (known-cells %)) (keys known-cells)))

(defn- parse-args [args]
  (loop [args args opts {:paths []}]
    (if-let [a (first args)]
      (case a
        "--cells" (recur (nnext args)
                         (assoc opts :cells (mapv keyword (str/split (second args) #","))))
        "--out" (recur (nnext args) (assoc opts :out (second args)))
        (recur (next args) (update opts :paths conj a)))
      opts)))

(defn- under? [^java.io.File dir path]
  (.startsWith (.toPath (.getCanonicalFile (io/file path)))
               (.toPath (.getCanonicalFile dir))))

(defn- default-root
  "What case ids are relative to: conformance/ when every path is inside it,
  as they are for the suite itself; otherwise the first path, or its directory."
  [paths]
  (let [suite (io/file "conformance")
        ^java.io.File first-path (io/file (first paths))]
    (cond
      (every? #(under? suite %) paths) suite
      (.isDirectory first-path) first-path
      :else (.getParentFile (.getAbsoluteFile first-path)))))

(defn run
  "Run the suite and return the report data. `cells` are cell ids."
  [{:keys [cells paths root]}]
  (let [ids (or cells (default-cells))
        unknown (remove known-cells ids)]
    (when (seq unknown)
      (throw (ex-info (str "unknown cells " (vec unknown) "; known: "
                           (str/join ", " (map name (keys known-cells))))
                      {})))
    (let [opened (atom [])]
      (try
        (doseq [id ids]
          (swap! opened conj ((requiring-resolve (known-cells id)))))
        (let [paths (if (seq paths) paths ["conformance"])]
          (runner/run {:root (or (some-> root io/file) (default-root paths))
                       :paths paths
                       :cells @opened}))
        (finally
          (run! cell/-close @opened))))))

(defn- compile-runtime!
  "Compile the kernel's Java when a source changed, as the test runner does,
  so the JVM cells never run stale classes. Only from the repository root,
  where the suite is run."
  []
  (when (.exists (io/file "runtime/build.clj"))
    (load-file "runtime/build.clj")
    ((requiring-resolve 'build/compile) {})))

(defn -main [& args]
  (compile-runtime!)
  (let [{:keys [out] :as opts} (parse-args args)
        report (run opts)
        path (report/write-edn! report (or out "target/conformance/report.edn"))]
    (print (report/text report))
    (println "Report data:" path)
    (flush)
    (shutdown-agents)
    (System/exit (if (report/ok? report) 0 1))))
