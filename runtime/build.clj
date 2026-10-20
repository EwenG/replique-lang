(ns build
  "Compiles the host kernel's Java sources, src/jvm, to target/classes.

    clojure -X:deps prep          once, from the repository root: the runtime
                                  is a :deps/prep-lib, so a fresh checkout
                                  compiles before its first use
    (build/compile {})            what prep calls, and what the test runner
                                  calls first, so an edited .java is never
                                  run stale

  Only javax.tools, no tools.build, so the test runner can call it without
  another dependency. Compiles for the running JDK's release until JDK 27,
  the baseline (doc/compilation.md §1.3), is installed."
  (:refer-clojure :exclude [compile])
  (:require [clojure.java.io :as io])
  (:import [javax.tools ToolProvider]))

(def ^:private here
  "This module's directory, whatever the working directory: the file this
  namespace was loaded from."
  (.getParentFile (io/file *file*)))

(defn- sources [] (filter #(.endsWith (.getName ^java.io.File %) ".java")
                          (file-seq (io/file here "src/jvm"))))

(defn- stale? [out]
  (let [marker (io/file out ".compiled")]
    (or (not (.exists marker))
        (some #(> (.lastModified ^java.io.File %) (.lastModified marker)) (sources)))))

(defn compile
  "Compile src/jvm when a source is newer than the last compile. Throws with
  javac's messages when it fails."
  [_]
  (let [out (io/file here "target/classes")]
    (when (stale? out)
      (.mkdirs out)
      (let [err (java.io.ByteArrayOutputStream.)
            args (into ["-d" (str out) "--release" (str (.feature (Runtime/version)))
                        "-Xlint:all" "-Xlint:-serial" "-Werror"]
                       (map str (sources)))
            status (.run (ToolProvider/getSystemJavaCompiler)
                         nil nil err (into-array String args))]
        (when-not (zero? status)
          (throw (ex-info (str "javac failed:\n" err) {:status status})))
        (spit (io/file out ".compiled") "")))
    nil))
