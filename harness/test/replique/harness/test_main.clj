(ns replique.harness.test-main
  "clojure -M:test: every test namespace of the runtime, the compiler, the
  tooling and the harness. The runtime's Java is compiled first when a source changed, so a
  test never runs stale classes."
  (:require [clojure.test :as t]))

(def namespaces
  '[replique.lang.kernel-test
    replique.reader-test
    replique.analyzer-test
    replique.jvm-test
    replique.js-test
    replique.recording-test
    replique.tooling.server-test
    replique.harness.cases-test
    replique.harness.analysis-test
    replique.harness.canon-test
    replique.harness.kernel-print-test
    replique.harness.runner-test
    replique.harness.oracle-test
    replique.harness.recorded-test])

(defn -main [& _]
  (load-file "runtime/build.clj")
  ((requiring-resolve 'build/compile) {})
  (apply require namespaces)
  (let [{:keys [fail error]} (apply t/run-tests namespaces)]
    (shutdown-agents)
    (System/exit (if (zero? (+ fail error)) 0 1))))
