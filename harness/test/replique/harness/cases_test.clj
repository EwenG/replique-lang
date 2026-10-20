(ns replique.harness.cases-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [replique.harness.cases :as cases])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory "harness-test" (make-array FileAttribute 0))))

(defn- read-src
  "Write `src` as sub/x.rpq under a fresh root and read it back."
  [src]
  (let [root (temp-dir)
        f (io/file root "sub" "x.rpq")]
    (io/make-parents f)
    (spit f src)
    (cases/read-file root f)))

(deftest a-file-is-a-namespace-and-cases
  (let [{:keys [ns rel cases]} (read-src "(ns a.b)\n\n(expect 2 (if nil 1 2))\n")]
    (is (= 'a.b ns))
    (is (= "sub/x.rpq" rel))
    (is (= 1 (count cases)))
    (let [c (first cases)]
      (is (= "sub/x.rpq:3" (:id c)))
      (is (= "(if nil 1 2)" (:name c)))
      (is (= {:line 3 :column 1 :end-line 3 :end-column 24} (:span c)))
      (is (= cases/all-modes (:modes c)))
      (is (= cases/all-targets (:targets c)))
      (is (false? (:clojure? c)))
      (is (= [{:op :expect :expected 2 :expr '(if nil 1 2) :id "sub/x.rpq:3"
               :span (:span c)}]
             (:steps c))))))

(deftest metadata-restricts-a-case
  (let [[c] (:cases (read-src "(ns a)\n^{:modes #{:dev} :targets #{:js} :clojure true}\n(expect 1 1)"))]
    (is (= #{:dev} (:modes c)))
    (is (= #{:js} (:targets c)))
    (is (true? (:clojure? c)))))

(deftest a-scenario-has-forms-and-checks
  (let [[c] (:cases (read-src "(ns a)\n(scenario \"s\"\n  (def x 1)\n  (expect 1 x)\n  (expect-throws :ex-info (f)))"))]
    (is (= "s" (:name c)))
    (is (= [:form :expect :expect-throws] (mapv :op (:steps c))))
    (testing "checks inside a scenario are numbered by line"
      (is (= ["sub/x.rpq:2/4" "sub/x.rpq:2/5"] (map :id (cases/checks c)))))
    (testing "forms keep their own position"
      (is (= {:line 3 :column 3} (:span (first (:steps c))))))))

(defmacro ^:private are-errors [& pairs]
  `(do ~@(for [[src re] (partition 2 pairs)]
           `(is (~'thrown-with-msg? clojure.lang.ExceptionInfo ~re (read-src ~src))))))

(deftest malformed-case-files-are-errors
  (are-errors
   "(expect 1 1)" #"starts with an ns form"
   "(ns a)\n(println 1)" #"expected expect"
   "(ns a)\n(expect 1)" #"takes two arguments"
   "(ns a)\n^:clojur (expect 1 1)" #"unknown case options"
   "(ns a)\n^{:modes #{:fast}} (expect 1 1)" #":modes must be"
   "(ns a)\n(scenario x (expect 1 1))" #"needs a name"
   "(ns a)\n(expect-error 1 (f))" #"diagnostic code"))

(deftest reader-conditionals-resolve-per-target
  (let [[c] (:cases (read-src "(ns a)\n(expect #?(:jvm 1/2 :js 0.5) [1 #?@(:js [2 3]) #?(:jvm 4)])"))
        {:keys [expected expr]} (first (:steps c))]
    (is (= 1/2 (cases/resolve-conditionals expected :jvm)))
    (is (= 0.5 (cases/resolve-conditionals expected :js)))
    (is (= [1 4] (cases/resolve-conditionals expr :jvm)))
    (is (= [1 2 3] (cases/resolve-conditionals expr :js))))
  (testing ":default, and metadata is kept"
    (let [form (cases/resolve-conditionals
                (read-string {:read-cond :preserve} "^:m (f #?(:js 1 :default 2))") :jvm)]
      (is (= '(f 2) form))
      (is (:m (meta form))))))

(deftest case-files-are-found-and-sorted
  (let [root (temp-dir)]
    (doseq [p ["b.rpq" "a/c.rpq" "a/notes.md"]]
      (let [f (io/file root p)] (io/make-parents f) (spit f "(ns x)")))
    (is (= ["a/c.rpq" "b.rpq"]
           (map #(subs (.getPath %) (inc (count (.getPath root))))
                (cases/case-files [root]))))))
