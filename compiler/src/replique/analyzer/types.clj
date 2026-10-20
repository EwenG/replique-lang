(ns replique.analyzer.types
  "The type lattice of doc/ir.md §2.

  A type is a map {:base b :nil? bool}. The base is one of:

    :any            nothing is known
    :never          the expression doesn't return
    a primitive     a symbol, JVM only: long double boolean int float char
                    short byte
    a host class    a java.lang.Class on the JVM; in JS one of the symbols
                    number string boolean bigint, or {:js/ctor sym}
    a core kind     :vector :map :set :list :keyword :symbol :fn :uuid

  nil is {:base :never :nil? true}: the type of an expression whose only
  value is nil, which joins with anything to make it nilable.")

(def any {:base :any :nil? true})
(def never {:base :never :nil? false})
(def nil-type {:base :never :nil? true})

(def primitives '#{long double boolean int float char short byte})

(def ^:private boxes
  {'long Long 'double Double 'boolean Boolean 'int Integer 'float Float
   'char Character 'short Short 'byte Byte})

(defn of
  "The type of a value of base `base`, which can't be nil unless `nil?`."
  ([base] (of base false))
  ([base nil?]
   (if (and nil? (primitives base))
     {:base (boxes base) :nil? true}
     {:base base :nil? (boolean nil?)})))

(defn nilable [t] (of (:base t) true))

(defn- box [base] (get boxes base base))

(defn- common-superclass [^Class a ^Class b]
  (loop [^Class c a]
    (cond
      (nil? c) :any
      (= Object c) :any
      (.isAssignableFrom c b) c
      :else (recur (.getSuperclass c)))))

(defn- numeric? [base]
  (and (class? (box base)) (.isAssignableFrom Number ^Class (box base))))

(defn- join-base [a b]
  (cond
    (= a b) a
    (= :never a) b
    (= :never b) a
    (or (= :any a) (= :any b)) :any
    (and (primitives a) (primitives b) (numeric? a) (numeric? b)) Number
    (and (class? (box a)) (class? (box b))) (common-superclass (box a) (box b))
    :else :any))

(defn join
  "The most precise type covering both `a` and `b` (§2.2)."
  ([] never)
  ([a] a)
  ([a b]
   (let [base (join-base (:base a) (:base b))
         nil? (or (:nil? a) (:nil? b))]
     (of base nil?)))
  ([a b & more] (reduce join (join a b) more)))

(defn primitive? [t] (contains? primitives (:base t)))
