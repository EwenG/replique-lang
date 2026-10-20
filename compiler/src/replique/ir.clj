(ns replique.ir
  "Walking the IR of doc/ir.md generically: every node names the keys that
  hold its children under :children, in evaluation order, so these functions
  work on every node type without knowing them.")

(defn children
  "The child nodes of `node`, in evaluation order. A key may hold one node,
  a vector of nodes, or nil. A map literal's keys and values are evaluated
  interleaved, in source order (doc/ir.md §5.1)."
  [node]
  (if (= :map (:op node))
    (vec (interleave (:keys node) (:vals node)))
    (reduce (fn [acc k]
              (let [v (get node k)]
                (cond (nil? v) acc
                      (vector? v) (into acc v)
                      :else (conj acc v))))
            []
            (:children node))))

(defn update-children
  "`node` with `f` applied to each child."
  [node f]
  (reduce (fn [n k]
            (let [v (get n k)]
              (cond (nil? v) n
                    (vector? v) (assoc n k (mapv f v))
                    :else (assoc n k (f v)))))
          node
          (:children node)))

(defn postwalk
  "Rebuild the tree bottom up: `f` sees each node after its children."
  [node f]
  (f (update-children node #(postwalk % f))))

(defn prewalk
  "Rebuild the tree top down: `f` sees each node before its children."
  [node f]
  (update-children (f node) #(prewalk % f)))

(defn nodes
  "Every node of the tree, parents before children, in evaluation order."
  [node]
  (tree-seq (comp seq :children) children node))
