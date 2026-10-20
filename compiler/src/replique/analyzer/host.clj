(ns replique.analyzer.host
  "The host resolvers of doc/ir.md §3.4: what the analyzer asks of the target,
  so that the analyzer itself stays host-neutral. One per target.

  On the JVM, classes and members are resolved at compile time through
  reflection on the compiler's class loader: a member is
  {:class C :name n :descriptor d :static? b :interface? b}, the form the
  ClassFile API needs, or :reflective when it can't be chosen. In JS there is
  nothing to resolve: a member is a name, and every host value has type :any."
  (:require [clojure.string :as str]
            [replique.analyzer.types :as t])
  (:import [java.lang.invoke MethodType]
           [java.lang.reflect Constructor Field Method Modifier]
           [java.time Instant]
           [java.util UUID]
           [java.util.regex Pattern]))

(defprotocol Host
  (target [h] "the target, :jvm or :js")
  (resolve-class [h imports sym]
    "the class `sym` names, given the namespace's imports, or nil")
  (hint-type [h imports tag]
    "the type a ^tag stands for, or nil if the tag names nothing")
  (value-type [h v] "the type of the constant `v`")
  (constructor [h cls arg-types] "{:member m} for `new`")
  (method [h cls method-name arg-types static?]
    "{:member m :type t} for a method call; `cls` nil when the target's
    class isn't known")
  (field [h cls field-name static?] "{:member m :type t} for a field read")
  (members [h cls member-name kind]
    "every overload a qualified method may stand for: kind is :instance,
    :static or :ctor"))

(defn- kind-of [v]
  (cond
    (vector? v) :vector (map? v) :map (set? v) :set (seq? v) :list
    (keyword? v) :keyword (symbol? v) :symbol))

;; --- the JVM ---------------------------------------------------------------

(def ^:private primitive-classes
  {'long Long/TYPE 'double Double/TYPE 'boolean Boolean/TYPE 'int Integer/TYPE
   'float Float/TYPE 'char Character/TYPE 'short Short/TYPE 'byte Byte/TYPE
   'void Void/TYPE})

(def ^:private array-hints
  {'longs "[J" 'doubles "[D" 'booleans "[Z" 'ints "[I" 'floats "[F"
   'chars "[C" 'shorts "[S" 'bytes "[B" 'objects "[Ljava.lang.Object;"})

(def ^:private boxes
  {'long Long 'double Double 'boolean Boolean 'int Integer 'float Float
   'char Character 'short Short 'byte Byte})

(defn- class-type
  "The type of a value of class `c`: a primitive can't be nil, an object can."
  [^Class c]
  (cond
    (= Void/TYPE c) t/nil-type
    (.isPrimitive c) (t/of (symbol (.getName c)))
    :else (t/of c true)))

(defn- load-class [^ClassLoader loader ^String n]
  (try (Class/forName n false loader)
       (catch ClassNotFoundException _ nil)
       (catch LinkageError _ nil)))

(defn- descriptor [^Class ret params]
  (.toMethodDescriptorString (MethodType/methodType ret ^"[Ljava.lang.Class;" (into-array Class params))))

(defn- accepts?
  "Whether a parameter of class `p` accepts an argument of type `a`.
  `boxing?` allows a primitive to be boxed and an object to be unboxed; an
  argument of unknown type is then accepted by anything, as the call is made
  through a cast."
  [boxing? ^Class p a]
  (let [base (:base a)]
    (cond
      (= :any base) (or boxing? (not (.isPrimitive p)))
      (= :never base) (or (not (:nil? a)) (not (.isPrimitive p)))
      (symbol? base) (or (= p (primitive-classes base))
                         (and boxing? (not (.isPrimitive p))
                              (.isAssignableFrom p ^Class (boxes base))))
      (class? base) (if (.isPrimitive p)
                      (and boxing? (= base (boxes (symbol (.getName p)))))
                      (.isAssignableFrom p ^Class base))
      :else (not (.isPrimitive p)))))

(defn- more-specific? [params-a params-b]
  (every? true? (map (fn [^Class a ^Class b] (or (= a b) (.isAssignableFrom b a)))
                     params-a params-b)))

(defn- choose
  "The candidate Java's overload rules would choose for `arg-types`, or
  :reflective: the most specific of those that fit without boxing, else of
  those that fit with it."
  [candidates params-of arg-types]
  (if (= 1 (count candidates))
    (first candidates)
    (let [fits (fn [boxing?] (filter #(every? true? (map (partial accepts? boxing?) (params-of %) arg-types))
                                     candidates))
          fit (or (seq (fits false)) (fits true))
          best (filter (fn [m] (every? #(more-specific? (params-of m) (params-of %)) fit)) fit)]
      (if (= 1 (count best)) (first best) :reflective))))

(defn- method-member [^Method m]
  {:class (.getDeclaringClass m) :name (.getName m)
   :descriptor (descriptor (.getReturnType m) (.getParameterTypes m))
   :static? (Modifier/isStatic (.getModifiers m))
   :interface? (.isInterface (.getDeclaringClass m))})

(deftype JvmHost [^ClassLoader loader]
  Host
  (target [_] :jvm)
  (resolve-class [_ imports sym]
    (when (and (symbol? sym) (nil? (namespace sym)))
      (let [n (name sym)]
        (or (some->> (get imports sym) str (load-class loader))
            (when (str/includes? n ".") (load-class loader n))
            (when-not (str/includes? n ".") (load-class loader (str "java.lang." n)))))))
  (hint-type [h imports tag]
    (let [tag (if (string? tag) (symbol tag) tag)]
      (cond
        (not (symbol? tag)) nil
        (primitive-classes tag) (class-type (primitive-classes tag))
        (array-hints tag) (t/of (load-class loader (array-hints tag)) true)
        :else (some-> (resolve-class h imports tag) (t/of true)))))
  (value-type [_ v]
    (cond
      (nil? v) t/nil-type
      (boolean? v) (t/of 'boolean)
      (instance? Long v) (t/of 'long)
      (instance? Double v) (t/of 'double)
      (char? v) (t/of 'char)
      (kind-of v) (t/of (kind-of v))
      :else (t/of (class v))))
  (constructor [_ cls arg-types]
    (let [^Class cls cls
          cs (filter #(= (count arg-types) (.getParameterCount ^Constructor %))
                     (.getConstructors cls))
          c (choose cs #(.getParameterTypes ^Constructor %) arg-types)]
      {:member (if (= :reflective c)
                 c
                 {:class cls :name "<init>"
                  :descriptor (descriptor Void/TYPE (.getParameterTypes ^Constructor c))
                  :static? false :interface? false})
       :type (t/of cls)}))
  (method [_ cls method-name arg-types static?]
    (if (nil? cls)
      {:member :reflective :type t/any}
      (let [^Class cls cls
            ms (filter #(and (= (name method-name) (.getName ^Method %))
                             (= (count arg-types) (.getParameterCount ^Method %))
                             (= (boolean static?) (Modifier/isStatic (.getModifiers ^Method %)))
                             (not (.isBridge ^Method %)))
                       (.getMethods cls))
            m (choose ms #(.getParameterTypes ^Method %) arg-types)]
        (if (= :reflective m)
          {:member :reflective
           :type (let [rets (distinct (map #(.getReturnType ^Method %) ms))]
                   (if (= 1 (count rets)) (class-type (first rets)) t/any))
           :none? (empty? ms)}
          {:member (method-member m) :type (class-type (.getReturnType ^Method m))}))))
  (field [_ cls field-name static?]
    (if (nil? cls)
      {:member :reflective :type t/any}
      (let [^Field f (try (.getField ^Class cls (name field-name))
                          (catch NoSuchFieldException _ nil))]
        (if (and f (= (boolean static?) (Modifier/isStatic (.getModifiers f))))
          {:member {:class (.getDeclaringClass f) :name (.getName f)
                    :descriptor (.descriptorString (.getType f))
                    :static? (boolean static?) :interface? false}
           :type (class-type (.getType f))}
          {:member :reflective :type t/any :none? true}))))
  (members [_ cls member-name kind]
    (let [^Class cls cls]
      (case kind
        :ctor (mapv (fn [^Constructor c]
                      {:class cls :name "<init>"
                       :descriptor (descriptor Void/TYPE (.getParameterTypes c))
                       :static? false :interface? false})
                    (.getConstructors cls))
        (->> (.getMethods cls)
             (filter #(and (= (name member-name) (.getName ^Method %))
                           (= (= :static kind) (Modifier/isStatic (.getModifiers ^Method %)))
                           (not (.isBridge ^Method %))))
             (mapv method-member))))))

(defn jvm
  "The JVM resolver, loading classes through `loader`, by default the
  compiler's own."
  ([] (jvm (.getContextClassLoader (Thread/currentThread))))
  ([loader] (->JvmHost loader)))

;; --- JS --------------------------------------------------------------------

(def ^:private js-hints '#{number string boolean bigint})

(deftype JsHost []
  Host
  (target [_] :js)
  (resolve-class [_ _ _] nil)
  (hint-type [_ _ tag]
    (let [tag (if (string? tag) (symbol tag) tag)]
      (cond
        (js-hints tag) (t/of tag true)
        (= 'js tag) t/any
        :else nil)))
  (value-type [_ v]
    (cond
      (nil? v) t/nil-type
      (boolean? v) (t/of 'boolean)
      (instance? clojure.lang.BigInt v) (t/of 'bigint)
      (number? v) (t/of 'number)
      (or (string? v) (char? v)) (t/of 'string)
      (instance? Pattern v) (t/of {:js/ctor 'RegExp})
      (instance? Instant v) (t/of {:js/ctor 'Date})
      (instance? UUID v) (t/of :uuid)
      (kind-of v) (t/of (kind-of v))
      :else t/any))
  (constructor [_ _ _] {:member nil :type t/any})
  (method [_ _ _ _ _] {:member nil :type t/any})
  (field [_ _ _ _] {:member nil :type t/any})
  (members [_ _ _ _] nil))

(defn js [] (->JsHost))
