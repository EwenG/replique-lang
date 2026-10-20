(ns replique.tooling.scope
  "Which functions are recorded, chosen (doc/server.md §5): a function with
  the ones around it, `:recording-scope`, and the ones the code recorded
  calls, by their calls only, `:record-calls`. Ported from replique's
  replique.recording, whose protocol this speaks.

  WHAT A DEFINITION CALLS is what the compiler says the form defining it
  used, the vars it resolved: kept here of each var as it was last defined
  on a target, with the file it was defined in. A var defined in a file of
  the project, under the server's directory, is the project's. Any other,
  defined in a file elsewhere or at a prompt, is a library's to the
  project: its code is not read through, and it is recorded by its calls
  only. Never replique's own, replique.core called everywhere.

  THE CODE OF THE PROJECT IS RECORDED FORM BY FORM, evaluated again to be,
  by the client, from its files, since what it evaluates is what it shows:
  what is decided here is which definitions are around a function, and in
  which files."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [replique.analyzer.env :as env]
            [replique.tooling.recording :as recording :refer [refuse]])
  (:import [java.io File IOException]))

(defn uses
  "What the definitions of a target use: {q {:uses #{q} :file}}."
  []
  (atom {}))

(defn used!
  "The record `r` ran on the target `target`: what the vars it defines use
  is kept, as they are defined now."
  [{:keys [uses cenv]} r]
  (when-let [qs (seq (:defines r))]
    (let [used (reduce disj (set (get-in r [:inputs :vars])) qs)]
      (swap! uses into (for [q qs]
                         [q {:uses used :file (get-in (env/find-var cenv q) [:span :file])}])))))

(defn- ours?
  "Whether the var `q` of what `uses` says is defined in a file on disk
  under the directory `directory`, a canonical path."
  [uses ^String directory q]
  (boolean
   (when-let [file (:file (get uses q))]
     (let [f (io/file file)
           path (try (.getCanonicalPath f) (catch IOException _ nil))]
       (and path (.isFile f) (str/starts-with? path (str directory File/separator)))))))

(defn- never
  "Why the calls of the var `q` are never recorded, or nil."
  [q]
  (when (str/starts-with? (namespace q) "replique.")
    "called everywhere, or what recording is made of"))

(defn- qualified [x]
  (let [s (when (or (string? x) (symbol? x) (keyword? x))
            (symbol (if (keyword? x) (subs (str x) 1) (str x))))]
    (if (qualified-symbol? s)
      s
      (refuse :invalid-message "A var is named by its qualified name, got: " (pr-str x)))))

(defn- qualified-list [k xs]
  (when-not (or (nil? xs) (sequential? xs))
    (refuse :invalid-message "The " (name k) " of a recording are a list, got: " (pr-str xs)))
  (mapv qualified xs))

(defn- around
  "The vars of the project around the var `q`, of what `uses` says,
  {:callers :callees}: those `depth` calls away at most, up for the
  callers and down for the callees."
  [uses q depth ours?]
  (let [callers (fn [x] (for [[k {u :uses}] uses :when (contains? u x)] k))
        callees (fn [x] (:uses (get uses x)))
        walk (fn [step]
               (loop [seen #{} frontier #{q} n depth]
                 (if (or (zero? n) (empty? frontier))
                   (disj seen q)
                   (let [next (into #{} (comp (mapcat step) (filter ours?) (remove seen)) frontier)]
                     (recur (into seen next) next (dec n))))))]
    {:callers (walk callers) :callees (walk callees)}))

(defn recording-scope
  "What `:recording-scope` answers, on the target `target`, the project's
  directory being `directory`: the var `:around` and the vars of the
  project `:depth` calls around it, 1 without, that call it or that it
  calls, each with the file it is defined in."
  [{:keys [uses cenv]} directory {q :around :keys [depth]}]
  (let [q (qualified q)
        depth (or depth 1)
        _ (when-not (pos-int? depth)
            (refuse :invalid-message "A :depth is a number of calls, got: " (pr-str depth)))
        _ (when-not (:defined? (env/find-var cenv q))
            (refuse :unknown-var q " is not defined: its file has to be loaded"))
        uses @uses
        project? #(ours? uses directory %)
        {:keys [callers callees]} (around uses q depth project?)]
    {:vars (vec (for [v (concat [q] (sort callers) (sort callees))
                      :when (project? v)]
                  {:var (str v) :file (:file (get uses v))}))
     :callers (mapv str (sort callers))
     :callees (mapv str (sort callees))}))

(defn record-calls
  "What `:record-calls` answers, with the recorder `r`, on the target
  `target`, the project's directory being `directory`: the vars `:vars`,
  and the ones of libraries the definitions of the vars `:callees-of` use,
  recorded by their calls only. Each `recorded` so, or `skipped`, with
  `why`."
  [r {:keys [uses] :as target} directory {:keys [vars callees-of]}]
  (let [named (qualified-list :vars vars)
        of (qualified-list :callees-of callees-of)
        uses @uses
        ;; of which replique's go without saying
        callees (into (sorted-set)
                      (comp (mapcat #(:uses (get uses %)))
                            (remove #(ours? uses directory %))
                            (remove never))
                      of)
        all (vec (distinct (concat named callees)))
        refused (into {} (keep (fn [q] (when-let [why (never q)] [q why]))) all)
        skipped (merge refused (recording/record-calls! r target (remove refused all)))]
    {:recorded (vec (for [q all :when (not (contains? skipped q))] (str q)))
     :skipped (vec (for [q all :let [why (get skipped q)] :when why]
                     {:var (str q) :why why}))}))
