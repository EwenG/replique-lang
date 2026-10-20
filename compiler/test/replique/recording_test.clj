(ns replique.recording-test
  "Code compiled to be recorded (doc/recording.md): the sites the pass
  chooses, and what the code tells the hook, the same on both targets.

  Each program runs with a :site that numbers sites in the order it is
  asked, and a hook that logs what it is told. A call is named c1, c2... in
  the order calls are entered. The JS half is skipped when node isn't
  installed."
  (:require [clojure.edn :as edn]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [replique.analyzer :as a]
            [replique.analyzer.sites :as sites]
            [replique.js.dev :as js]
            [replique.js.emit :as js-emit]
            [replique.jvm.dev :as jvm]
            [replique.jvm.emit :as jvm-emit])
  (:import [replique.lang Hook IFn Printer]))

(def ^:private node?
  (delay (try (zero? (:exit (sh/sh "node" "--version")))
              (catch java.io.IOException _ false))))

(defn- units [src target]
  (:units (a/analyze-source (a/environment) src {:file "t.rpq" :ns 'app.core :target target})))

(defn- sites-of
  "The sites of every form of `src`, in the order they were asked for, each
  with its span as [line column end-line end-column]."
  [src & {:keys [locals?]}]
  (let [out (atom [])]
    (doseq [r (units src :jvm)]
      (sites/mark r {:site (fn [m]
                             (swap! out conj (update m :span (juxt :line :column :end-line :end-column)))
                             (count @out))
                     :locals? locals?}))
    @out))

(defn- at [sites] (mapv :span sites))

(deftest the-sites-of-a-form
  (testing "the body, the fn's method, the call, the let: children first"
    (let [ss (sites-of "(let* [f (fn* ([x] (+ x 1)))] (f 41))")]
      (is (= [:value :enter :value :value] (map :kind ss)))
      (is (= [[1 20 1 27] [1 15 1 28] [1 31 1 37] [1 1 1 38]] (at ss)))
      (is (= '[x] (:params (second ss))))
      (is (= true (:outermost (second ss))))))
  (testing "a macro call reports at the call, and not what it expanded into"
    (let [ss (sites-of "(defn g [x] (when x (inc x)))")]
      (is (= [[1 21 1 28] [1 13 1 29] [1 1 1 30]] (at ss)))
      (is (= 'replique.core/when (:macro (second ss))))
      (is (= :enter (:kind (last ss))) "the defn is entered, at the defn")))
  (testing "a macro handing back forms the source wrote reports them, once each"
    (is (= [[1 7 1 12] [1 13 1 18]] (at (sites-of "(-> 1 (inc) (* 3))")))))
  (testing "a macro expanding into a call of itself reports once"
    (is (= [[1 7 1 14] [1 30 1 37] [1 23 1 38] [1 1 1 39]]
           (at (sites-of "(cond (= 1 2) 1 :else (and 1 (inc 2)))")))))
  (testing "what reports nothing: a def, a quote, a constant, a fn, declare"
    (is (= [:enter :enter :value]
           (map :kind (sites-of "(declare a b) (def x (quote (1 2))) (fn* ([] 1)) (defn f []) (do 1)")))))
  (testing "a fn inside a fn isn't the outermost"
    (is (= [false true] (map :outermost (filter #(= :enter (:kind %))
                                                (sites-of "(fn [x] (fn [y] y))"))))))
  (testing "a local site is a name a let or loop binds, the source's only"
    (let [ss (sites-of "(let [a 1 b (inc a)] (loop [i 0] (if (and i (< i 2)) (recur (inc i)) b)))"
                       :locals? true)]
      (is (= '[a b i] (keep :name ss)))
      (is (= [[1 7 1 8] [1 11 1 12] [1 29 1 30]] (at (filter #(= :local (:kind %)) ss))))
      (is (not-any? #(= :local (:kind %)) (sites-of "(let [a 1] a)")) "and only with :locals?")))
  (testing "nil from :site is no site"
    (let [r (sites/mark (first (units "(inc 1)" :jvm)) {:site (constantly nil)})]
      (is (nil? (:site (:node r)))))))

;; --- what the hook is told -------------------------------------------------

(defn- jvm-recorded
  "`src` run on the JVM, compiled to be recorded: {:value :log}."
  [src locals?]
  (let [rt (jvm/runtime)
        log (atom [])
        k (atom 0)
        n (atom 0)
        show #(cond (instance? IFn %) "fn"
                    (instance? Throwable %) (str "ex:" (ex-message %))
                    :else (Printer/print %))
        hook (reify Hook
               (enter [_ site outer args]
                 (let [c (str "c" (swap! k inc))]
                   (swap! log conj (str c " enters " site (when outer (str " in " outer))
                                        " [" (str/join ";" (map show args)) "]"))
                   c))
               (value [_ site v call]
                 (swap! log conj (str site "=" (show v) (when call (str " in " call))))
                 v)
               (exit [_ call] (swap! log conj (str call " exits"))))
        rec {:site (fn [_] (swap! n inc)) :hook hook :locals? locals?}
        out (mapv #(try (jvm/run-record rt 'app.core % rec)
                        (catch Exception e {:thrown (ex-message e)}))
                  (units src :jvm))]
    {:value (let [l (peek out)] (if (contains? l :value) (show (:value l)) (:thrown l)))
     :log (str/join " | " @log)}))

(def ^:private js-rt (delay (js/runtime)))

(def ^:private js-hook
  "The hook, a global. It keeps what it is told raw; js-log prints it as
  the JVM's hook does: a fn as fn, an exception as ex:message."
  (str "(globalThis.$H ??= {log: [], k: 0,"
       " enter(n, o, a) { const c = 'c' + (++this.k);"
       " this.log.push([c + ' enters ' + n + (o ? ' in ' + o : '') + ' [', a, ']']); return c; },"
       " value(n, v, c) { this.log.push([n + '=', v, c ? ' in ' + c : '']); return v; },"
       " exit(c) { this.log.push([c + ' exits']); }})"))

(def ^:private js-log
  (str "(function ($rt) {"
       " const show = (v) => typeof v === 'function' ? 'fn'"
       " : v instanceof Error ? 'ex:' + v.message : $rt.print(v);"
       " const l = globalThis.$H.log.map(([p, v, s]) => s === undefined ? p"
       " : p + (p.endsWith('[') ? v.map(show).join(';') : show(v)) + s).join(' | ');"
       " globalThis.$H.log = []; globalThis.$H.k = 0; return l; })"))

(defn- js-recorded [src locals?]
  (let [n (atom 0)
        rec {:site (fn [_] (swap! n inc)) :hook js-hook :locals? locals?}
        out (atom [])]
    (a/analyze-source (a/environment) src
                      {:file "t.rpq" :ns 'app.core :target :js
                       :on-unit #(swap! out conj (js/run-record @js-rt 'app.core % "t.rpq" nil rec))})
    {:value (let [l (peek @out)] (or (:value l) (:message (:thrown l))))
     :log (edn/read-string (:value (js/run-js @js-rt js-log "log")))}))

(def ^:private programs
  "[program value log], and :locals when local sites are asked for."
  [["(let* [f (fn* ([x] (+ x 1)))] (f 41))"
    "42" "c1 enters 2 [41] | 1=42 in c1 | c1 exits | 3=42 | 4=42"]
   ;; a branch that recurs reports nothing: the if reports once, when the
   ;; loop is over
   ["(loop* [i 0] (if (< i 3) (recur (inc i)) i))"
    "3" "1=true | 2=1 | 1=true | 2=2 | 1=true | 2=3 | 1=false | 3=3 | 4=3"]
   ;; a throw ends the call, and the try reports nothing for (t)
   ["(defn t [] (throw (ex-info \"boom\" 7))) (try (t) (catch :default e (ex-message e)))"
    "\"boom\"" "c1 enters 2 [] | 1=ex:boom in c1 | c1 exits | 4=\"boom\" | 5=\"boom\""]
   ["(defn g [x] (when x (inc x))) (g 1)"
    "2" "c1 enters 3 [1] | 1=2 in c1 | 2=2 in c1 | c1 exits | 4=2"]
   ;; a fn is in the call it was made in, called there or once it is over
   ["(defn o [x] (let [f (fn [y] (* y 2))] (f x))) (o 4)"
    "8" "c1 enters 5 [4] | c2 enters 2 in c1 [4] | 1=8 in c2 | c2 exits | 3=8 in c1 | 4=8 in c1 | c1 exits | 6=8"]
   ["(defn mk [x] (fn [] (inc x))) ((mk 1))"
    "2" "c1 enters 3 [1] | c1 exits | 4=fn | c2 enters 2 in c1 [] | 1=2 in c2 | c2 exits | 5=2"]
   ;; the rest parameter last, a host array until core's seqs
   ["(defn v [a & xs] (+ a (count xs))) (v 1 2 3)"
    "3" "c1 enters 3 [1;#array[2 3]] | 1=2 in c1 | 2=3 in c1 | c1 exits | 4=3"]
   ;; a recur to the top of a fn is not another call
   ["(defn r [n] (if (< 0 n) (recur (dec n)) n)) (r 2)"
    "0" "c1 enters 4 [2] | 1=true in c1 | 2=1 in c1 | 1=true in c1 | 2=0 in c1 | 1=false in c1 | 3=0 in c1 | c1 exits | 5=0"]
   ;; a try in an argument, which the JVM compiles as a fn, reports once,
   ;; in the call around it
   ["(defn s [] (str 1 (try 2 (finally 3)))) (s)"
    "\"12\"" "c1 enters 3 [] | 1=2 in c1 | 2=\"12\" in c1 | c1 exits | 4=\"12\""]
   ;; the forms of a call that throws out report up to the throw
   ["(defn u [x] (inc x) (throw (ex-info \"out\" x))) (u 1)"
    "out" "c1 enters 3 [1] | 1=2 in c1 | 2=ex:out in c1 | c1 exits"]
   ;; a let's local once bound, a loop's at the top of each iteration
   ["(defn lc [m xs] (let [a m n (count xs)] (loop [i 0] (if (< i 2) (recur (inc i)) (+ a n))))) (lc 1 \"ab\")"
    "3" (str "c1 enters 11 [1;\"ab\"] | 1=1 in c1 | 2=2 in c1 | 3=2 in c1 | 4=0 in c1"
             " | 5=true in c1 | 6=1 in c1 | 4=1 in c1 | 5=true in c1 | 6=2 in c1 | 4=2 in c1"
             " | 5=false in c1 | 7=3 in c1 | 8=3 in c1 | 9=3 in c1 | 10=3 in c1 | c1 exits | 12=3")
    :locals]])

(deftest what-the-hook-is-told-on-the-jvm
  (doseq [[src value log locals] programs]
    (let [r (jvm-recorded src (= :locals locals))]
      (is (= value (:value r)) src)
      (is (= log (:log r)) src))))

(deftest what-the-hook-is-told-in-js
  (if-not @node?
    (println "node isn't installed: skipping the JS half of" (str *ns*))
    (doseq [[src value log locals] programs]
      (let [r (js-recorded src (= :locals locals))]
        (is (= value (:value r)) src)
        (is (= log (:log r)) src)))))

(deftest nothing-is-recorded-unasked
  (let [[r] (units "(defn g [x] (when x (inc x)))" :js)]
    (is (not (str/includes? (js-emit/form-js 'app.core (:node r)) "$hook"))))
  (let [[r] (units "(defn g [x] (when x (inc x)))" :jvm)
        plan (jvm-emit/form-class 'app.core (.getClassLoader IFn) (:node r))
        bytes (mapcat :bytes (tree-seq (comp seq :children) :children plan))]
    (is (not (str/includes? (String. (byte-array bytes) "ISO-8859-1") "replique/lang/Hook")))))
