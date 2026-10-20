(ns replique.analyzer.passes
  "The passes the analyzer runs on every top-level form, adding the keys
  doc/ir.md §5 marks *(pass)*. Each is a function from IR to IR."
  (:require [replique.ir :as ir]))

(defn mark-recurs
  "Give each :loop its :recurs?: whether a recur jumps to it."
  [root]
  (let [targets (into #{} (keep #(when (= :recur (:op %)) (:target %))) (ir/nodes root))]
    (ir/postwalk root (fn [n]
                        (if (= :loop (:op n))
                          (assoc n :recurs? (contains? targets (:id n)))
                          n)))))

(defn- bound-ids [n]
  (into #{} (keep #(when (= :binding (:op %)) (:id %))) (ir/nodes n)))

(defn mark-closed-over
  "Give each :fn its :closed-over: the ids of the outer bindings it uses, in
  ascending order, which the JVM back end turns into fields."
  [root]
  (ir/postwalk root (fn [n]
                      (if (= :fn (:op n))
                        (let [inner (bound-ids n)]
                          (assoc n :closed-over
                                 (->> (ir/nodes n)
                                      (keep #(when (= :local (:op %)) (:id %)))
                                      (remove inner)
                                      distinct
                                      sort
                                      vec)))
                        n))))

(defn run
  "Every pass, in order."
  [root]
  (-> root mark-recurs mark-closed-over))
