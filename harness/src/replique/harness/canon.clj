(ns replique.harness.canon
  "Canonical printed data (doc/conformance.md §3).

  Results are compared as text in one canonical form, so that a result computed
  in a JVM and one read back from a JS runtime can be compared, and so that a
  failure shows both sides as data.

  The rules:

  - maps and sets print with their entries sorted by their printed form;
  - numbers print with their host type: 1, 1.0, 1/2, 1N, 1M on the JVM. In JS
    every number is a double, so for the :js target a double with no fractional
    part prints as an integer: 1.0 and 1 are the same JS number, and an
    expectation that says 1.0 means it;
  - a var prints as (var ns/name), which is what #'ns/name reads as;
  - a function prints as #object[fn], any other host object as
    #object[class-name], and an array as #array[...].

  The values here are JVM values: a result from a Replique runtime has been
  printed there by the kernel's printer (replique.lang.Printer, runtime.js)
  and read back with read-printed before it gets here."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [replique.reader :as reader])
  (:import [clojure.lang BigInt IPersistentMap IPersistentSet Ratio TaggedLiteral Var]))

(def ^:private max-safe-integer 9007199254740991)

(declare canonical-str)

(defn- seq-str [open close items target]
  (str open (str/join " " (map #(canonical-str % target) items)) close))

(defn- sorted-strs [strs]
  (sort strs))

(defn- double-str [^double d target]
  (cond
    (Double/isNaN d) "##NaN"
    (Double/isInfinite d) (if (pos? d) "##Inf" "##-Inf")
    (and (= :js target) (== d (Math/rint d)) (<= (Math/abs d) max-safe-integer))
    (str (long d))
    :else (str d)))

(defn canonical-str
  "`x` in canonical printed form, for `target` (:jvm or :js)."
  [x target]
  (cond
    (nil? x) "nil"
    (boolean? x) (str x)
    (string? x) (pr-str x)
    (char? x) (pr-str x)
    (keyword? x) (str x)
    (symbol? x) (str x)

    (instance? Double x) (double-str x target)
    (instance? Float x) (double-str (double x) target)
    (integer? x) (cond
                   (instance? BigInt x) (str x "N")
                   (instance? java.math.BigInteger x) (str x "N")
                   :else (str x))
    (instance? Ratio x) (str x)
    (decimal? x) (str x "M")

    (instance? Var x) (let [^Var v x] (str "(var " (.-ns v) "/" (.-sym v) ")"))
    (instance? TaggedLiteral x) (str "#" (:tag x) (canonical-str (:form x) target))
    (fn? x) "#object[fn]"

    (instance? IPersistentMap x)
    (str "{" (str/join ", " (sorted-strs (map (fn [[k v]]
                                                (str (canonical-str k target) " "
                                                     (canonical-str v target)))
                                              x)))
         "}")
    (instance? IPersistentSet x)
    (str "#{" (str/join " " (sorted-strs (map #(canonical-str % target) x))) "}")
    (vector? x) (seq-str "[" "]" x target)
    (seq? x) (seq-str "(" ")" x target)
    (.isArray (class x)) (seq-str "#array[" "]" (seq x) target)

    :else (str "#object[" (.getName (class x)) "]")))

(def ^:private printed-tags
  "The tags the kernel's printer writes for what has no literal."
  {'object #(tagged-literal 'object %) 'array #(tagged-literal 'array %)})

(defn read-printed
  "What a Replique runtime for `target` printed with the kernel's printer, as
  JVM data for canonical-str: read with Replique's reader, numbers as the
  target reads them, #object and #array as tagged literals."
  [text target]
  (reader/read-one text {:target target :tags printed-tags}))

(defn js-value
  "A value read back from a JS runtime, normalised wherever it is nested:
  ClojureScript prints a function as #object[Function name], which becomes the
  #object[fn] a JVM function prints as."
  [x]
  (walk/postwalk
   (fn [v]
     (if (and (instance? TaggedLiteral v)
              (= 'object (:tag v))
              (vector? (:form v))
              (= 'Function (first (:form v))))
       (tagged-literal 'object '[fn])
       v))
   x))
