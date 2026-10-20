(ns replique.tooling.json
  "The JSON the server writes, and reads back in its tests. One value per
  line: strings escape every control character, so a newline is always a
  frame boundary (doc/server.md §1).

  Keywords and symbols are written as their names, namespace included; nil
  is null. Nothing else needs JSON here: a value of the program travels as
  the text its printer wrote."
  (:require [clojure.string :as str]))

(defn- write-string [^StringBuilder sb ^String s]
  (.append sb \")
  (dotimes [i (.length s)]
    (let [c (.charAt s i)]
      (case c
        \" (.append sb "\\\"")
        \\ (.append sb "\\\\")
        \newline (.append sb "\\n")
        \return (.append sb "\\r")
        \tab (.append sb "\\t")
        (if (or (< (int c) 0x20) (= (int c) 0x2028) (= (int c) 0x2029))
          (.append sb (format "\\u%04x" (int c)))
          (.append sb c)))))
  (.append sb \"))

(defn- named [x]
  (if-let [n (namespace x)] (str n "/" (name x)) (name x)))

(defn- write-value [^StringBuilder sb x]
  (cond
    (nil? x) (.append sb "null")
    (boolean? x) (.append sb (str x))
    (string? x) (write-string sb x)
    (or (keyword? x) (symbol? x)) (write-string sb (named x))
    (integer? x) (.append sb (str x))
    (number? x) (let [d (double x)]
                  (if (or (Double/isNaN d) (Double/isInfinite d))
                    (throw (ex-info (str "JSON has no " x) {:value x}))
                    (.append sb (str d))))
    (map? x) (do (.append sb \{)
                 (doseq [[i [k v]] (map-indexed vector x)]
                   (when (pos? i) (.append sb \,))
                   (write-string sb (if (string? k) k (named k)))
                   (.append sb \:)
                   (write-value sb v))
                 (.append sb \}))
    (sequential? x) (do (.append sb \[)
                        (doseq [[i v] (map-indexed vector x)]
                          (when (pos? i) (.append sb \,))
                          (write-value sb v))
                        (.append sb \]))
    :else (throw (ex-info (str "JSON can't hold a " (type x)) {:value x}))))

(defn write-str
  "`x` as JSON, on one line."
  ^String [x]
  (let [sb (StringBuilder.)]
    (write-value sb x)
    (str sb)))

;; --- reading, for the tests -----------------------------------------------

(defn- skip-ws [^String s i]
  (loop [i i]
    (if (and (< i (count s)) (Character/isWhitespace (.charAt s i))) (recur (inc i)) i)))

(declare ^:private read-at)

(defn- read-string-at [^String s i]
  (let [sb (StringBuilder.)]
    (loop [i (inc i)]
      (let [c (.charAt s i)]
        (case c
          \" [(str sb) (inc i)]
          \\ (let [e (.charAt s (inc i))]
               (if (= e \u)
                 (do (.append sb (char (Integer/parseInt (subs s (+ i 2) (+ i 6)) 16)))
                     (recur (+ i 6)))
                 (do (.append sb (case e \n \newline \r \return \t \tab \b \backspace \f \formfeed e))
                     (recur (+ i 2)))))
          (do (.append sb c) (recur (inc i))))))))

(defn- read-seq-at [^String s i close read-item]
  (loop [i (skip-ws s (inc i)) out []]
    (if (= (int close) (int (.charAt s i)))
      [out (inc i)]
      (let [[x i] (read-item i)
            i (skip-ws s i)
            i (if (= \, (.charAt s i)) (skip-ws s (inc i)) i)]
        (recur i (conj out x))))))

(defn- read-at [^String s i]
  (let [i (skip-ws s i)
        c (.charAt s i)]
    (case c
      \" (read-string-at s i)
      \{ (let [[kvs i] (read-seq-at s i \}
                                     (fn [i]
                                       (let [[k i] (read-string-at s (skip-ws s i))
                                             i (skip-ws s i)
                                             [v i] (read-at s (inc i))]
                                         [[(keyword k) v] i])))]
           [(into {} kvs) i])
      \[ (read-seq-at s i \] #(read-at s %))
      \t [true (+ i 4)]
      \f [false (+ i 5)]
      \n [nil (+ i 4)]
      (let [end (or (some #(when (#{\, \} \] \space} (.charAt s %)) %) (range i (count s)))
                    (count s))
            t (subs s i end)]
        [(if (re-find #"[.eE]" t) (Double/parseDouble t) (Long/parseLong t)) end]))))

(defn read-str
  "The JSON `s`; object keys become keywords."
  [^String s]
  (first (read-at (str/trim s) 0)))
