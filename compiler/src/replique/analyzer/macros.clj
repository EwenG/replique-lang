(ns replique.analyzer.macros
  "The expander: calling a macro, recording the expansion (doc/ir.md §4.3),
  and the API a macro uses to talk to the compiler (doc/compilation.md §5).

  A macro is a var with :macro? true and a :macro-fn, called as Clojure's are,
  with &form, &env and the arguments. &env is a map from each local in scope
  to {:name :local :id}.

  Two kinds of macro, told apart by the var's :macro-kind:

    :bootstrap  written in Clojure, in the compiler, on the compiler's data
                (replique.analyzer.bootstrap). Called directly.
    :replique   written in Replique and compiled for the JVM. It works on
                Replique's own collections, so its arguments are copied into
                Replique data and its result copied back (doc/reader.md §4).

  The JVM back end compiles a :replique macro's var like any other, but the
  copy needs Replique's collections, so until core has them (M2) a :replique
  macro can be defined but not called."
  (:refer-clojure :exclude [gensym resolve])
  (:require [replique.analyzer.env :as env]
            [replique.analyzer.host :as host]
            [replique.analyzer.unit :as u])
  (:import [clojure.lang TaggedLiteral Var]
           [java.time Instant]
           [java.util UUID]
           [java.util.regex Pattern]))

(def ^:dynamic *context*
  "{:unit :env} while a macro runs, so the functions below know which form
  is being compiled. The one dynamic var of the compiler: a macro is code
  the compiler didn't write, and this is how it reaches back."
  nil)

(defn- context []
  (or *context*
      (throw (IllegalStateException. "only callable while a macro is expanding"))))

;; --- the API for macros ----------------------------------------------------

(defn gensym
  "A fresh symbol, numbered by the file's counter rather than a global one,
  so compiling the same file twice gives the same names."
  ([] (gensym "G"))
  ([prefix]
   (let [{:keys [unit]} (context)]
     (symbol (str prefix "__" (swap! (:gensym-counter unit) inc) "__auto__")))))

(defn current-ns
  "The namespace being compiled."
  []
  (:ns (:unit (context))))

(defn target
  "The target the code is compiled for, :jvm or :js."
  []
  (host/target (:host (:unit (context)))))

(defn var-info
  "The compile-time metadata of the var `sym` names in the current
  namespace, or nil. Recorded as an input of the form, so a change to the
  var recompiles it."
  [sym]
  (let [{:keys [unit]} (context)]
    (when-let [v (env/resolve-var (:cenv unit) (:ns unit) sym)]
      (u/input! unit :vars (:var v))
      (dissoc v :macro-fn))))

(defn depends-on-file!
  "Declare that the expansion read the file at `path` (doc/compilation.md §5.1)."
  [path]
  (u/input! (:unit (context)) :files (str path)))

(defn depends-on-env!
  "Declare that the expansion read the environment variable `name`."
  [name]
  (u/input! (:unit (context)) :env (str name)))

;; --- what an expansion may contain -----------------------------------------

(defn- embeddable-leaf? [x]
  (or (nil? x) (boolean? x) (number? x) (string? x) (char? x) (keyword? x)
      (symbol? x) (instance? Pattern x) (instance? UUID x) (instance? Instant x)
      (class? x)))

(defn unembeddable
  "The path to the first value in `form` that can't be emitted as a literal
  (doc/compilation.md §5.2), with the value: [path x], or nil."
  [form]
  (letfn [(walk [path x]
            (cond
              (embeddable-leaf? x) nil
              (instance? Var x) [path x]
              (instance? TaggedLiteral x) (walk (conj path :form) (:form x))
              (map? x) (or (some (fn [[k v]] (or (walk (conj path k) k) (walk (conj path k) v))) x)
                           (walk path (meta x)))
              (or (vector? x) (seq? x)) (or (first (keep-indexed #(walk (conj path %1) %2) x))
                                            (walk path (meta x)))
              (set? x) (or (some #(walk (conj path %) %) x)
                           (walk path (meta x)))
              :else [path x]))]
    (walk [] form)))

;; --- the boundary ----------------------------------------------------------

(defn- call-macro [info form locals]
  (case (:macro-kind info :bootstrap)
    :bootstrap (apply (:macro-fn info) form locals (rest form))
    ;; doc/reader.md §4: the copy into Replique data and back goes here, once
    ;; core's collections exist (M2)
    :replique (throw (ex-info "Replique macros need core's collections" {}))))

;; --- expanding -------------------------------------------------------------

(defn expand
  "Expand the macro call `form`, where the head resolved to the macro var
  `info`. `call` is the span the call has, `parent` the expansion it came
  from. Returns {:form expansion :id expansion-id}."
  [unit env form info call parent]
  (let [q (:var info)]
    (when-not (:macro-fn info)
      (u/fail call :analyzer/macro-not-loaded
              (str "the macro " q " can't be called yet: a macro written in Replique"
                   " takes its arguments as Replique data, which needs core's collections (M2)")))
    (u/input! unit :macros q)
    (let [id (u/next-id! unit)
          locals (into {} (map (fn [[s b]] [s (select-keys b [:name :local :id])]))
                       (:locals env))
          result (try
                   (binding [*context* {:unit unit :env env}]
                     (call-macro info form locals))
                   (catch clojure.lang.ExceptionInfo e
                     (if (u/diagnostic e)
                       (throw e)
                       (u/fail call :analyzer/macro-failed
                               (str "the macro " q " failed: " (ex-message e))
                               :data (ex-data e))))
                   (catch Exception e
                     (u/fail call :analyzer/macro-failed
                             (str "the macro " q " failed: " (or (ex-message e) (class e))))))]
      (when-let [[path x] (unembeddable result)]
        (u/fail call :analyzer/cannot-embed
                (str "the expansion of " q " contains " (if (instance? Var x) "a JVM var" (str "a " (.getName (class x))))
                     " at " (pr-str path) ", which can't be emitted as a literal")
                :path path))
      (u/add-expansion! unit id {:macro q :call call :parent parent})
      {:form result :id id})))
