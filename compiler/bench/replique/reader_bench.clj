(ns replique.reader-bench
  "clojure -M:bench: the reader against Clojure's, on large real files.

  Not a test: timings depend on the machine. The target (doc/reader.md §6) is
  a full read within about 2x of Clojure's reader."
  (:require [clojure.java.io :as io]
            [replique.reader :as r]
            [replique.reader.cst :as cst]))

(def files
  ["../../clojure/replique-clj/src/clj/clojure/core.clj"
   "../../clojure/replique-cljs/src/clj/cljs/core.cljs"])

(defn- clojure-read [^String src]
  (let [rdr (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. src))]
    (binding [*ns* (create-ns 'replique.reader-bench.scratch)
              *default-data-reader-fn* tagged-literal]
      (loop [n 0]
        (if (identical? ::eof (read {:eof ::eof :read-cond :allow :features #{:clj}} rdr))
          n
          (recur (inc n)))))))

(defn- ms
  "Mean milliseconds per call of `f`, after warming up."
  [f]
  (dotimes [_ 10] (f))
  (let [n 30 t (System/nanoTime)]
    (dotimes [_ n] (f))
    (/ (- (System/nanoTime) t) 1e6 n)))

(defn -main [& _]
  (doseq [f files :when (.exists (io/file f))]
    (let [src (slurp f)
          opts {:ns 'replique.reader-bench.scratch :core-ns 'clojure.core :features #{:clj}}
          clj (ms #(clojure-read src))
          tree (ms #(cst/parse src))
          full (ms #(r/read-source src opts))]
      (printf "%-10s %4d KB   clojure %6.1f ms   cst %6.1f ms   cst+forms %6.1f ms   %.1fx%n"
              (.getName (io/file f)) (quot (count src) 1000) clj tree full (/ full clj))))
  (flush)
  (shutdown-agents))
