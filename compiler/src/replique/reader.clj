(ns replique.reader
  "The reader (doc/reader.md): source text to a lossless CST, then to forms.

    (parse src opts)        the CST alone, for editors and formatters
    (read-source src opts)  the forms, with the CST they came from

  Options:

    :file            the file name, recorded in spans
    :target          :jvm (default) or :js; decides how numbers read
    :features        what reader conditionals select on, default #{target}
    :ns              the current namespace, for ::keywords and syntax-quote
    :aliases         {alias-symbol namespace-symbol}
    :resolve-symbol  (fn [ctx sym] qualified-sym), for syntax-quote
    :tags            {tag-symbol (fn [form] value)}, the namespace's data readers
    :special-forms   the symbols syntax-quote leaves unqualified
    :core-ns         where syntax-quote's list, concat, seq... live,
                     default replique.core
    :gensym-counter  an atom, so gensyms stay unique across reads"
  (:require [replique.reader.cst :as cst]
            [replique.reader.forms :as forms]))

(defn- with-span
  "A diagnostic with its offsets turned into a span."
  [parse {:keys [start end] :as d}]
  (-> d (dissoc :start :end) (assoc :span (cst/span parse start end))))

(defn parse
  "The lossless CST of `src`; see replique.reader.cst. Diagnostics have spans."
  ([src] (parse src nil))
  ([src opts]
   (let [p (cst/parse src opts)]
     (update p :diagnostics #(mapv (partial with-span p) %)))))

(defn read-source
  "Every top-level form of `src`:

    {:forms [{:form f :span s :text \"...\"} ...]
     :diagnostics [...]
     :incomplete? true when the only problem is that the source ended early
     :cst the parse}

  Never throws on bad input. A top-level form with an error is left out and
  its error reported; the forms around it are still read."
  ([src] (read-source src nil))
  ([src opts]
   (let [p (cst/parse src opts)
         ctx (forms/context p opts)
         results (for [node (:nodes p)
                       :let [[fs ds] (forms/top-level ctx node)]]
                   {:node node :forms fs :diagnostics ds})]
     {:forms (vec (for [{:keys [node forms]} results
                        f forms]
                    {:form f :span (cst/span p node) :text (cst/source-text p node)}))
      :diagnostics (mapv (partial with-span p)
                         (concat (:diagnostics p) (mapcat :diagnostics results)))
      :incomplete? (cst/incomplete? p)
      :cst p})))

(defn read-one
  "The first form of `src`, or throws the first error. For tests and the REPL's
  simple cases."
  ([src] (read-one src nil))
  ([src opts]
   (let [{:keys [forms diagnostics]} (read-source src opts)]
     (if-let [e (first (filter #(= :error (:severity %)) diagnostics))]
       (throw (ex-info (:message e) e))
       (:form (first forms))))))

(def element-span
  "See replique.reader.forms/element-span."
  forms/element-span)
