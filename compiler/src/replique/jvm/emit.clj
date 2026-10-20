(ns replique.jvm.emit
  "The JVM back end: the IR of doc/ir.md to class files, with the ClassFile
  API (doc/compilation.md §2.1). Pure: it returns bytes, and
  replique.jvm.dev defines and runs them.

  Every function is a class of its own, and so is every top-level form,
  compiled as a function of no arguments:

    public final class <ns>.<name> extends AFn, or RestFn when variadic
      private final Object c<id>     one field per local it closes over
      <init>(Object...)              takes them, in :closed-over order
      fnName()                       \"ns/name\", for arity errors
      invoke(Object...)              one per fixed arity
      requiredArity(), doInvoke(Object[])   the variadic arity

  A class is a plan, {:name :bytes :fields n :children [plan ...]}: the
  classes of the functions it creates are its children, compiled first. In
  dev mode they are hidden classes, which no other class can name, so a
  class creates a child through the child's constructor, a MethodHandle the
  loader puts in its class data: `ldc` of MethodHandles.classDataAt, then
  invokeExact with the values it closes over.

  Every value on the operand stack is an Object: primitives are boxed when a
  host member returns one and unboxed when a member takes one. Primitive
  arithmetic comes with the intrinsics.

  Vars link through replique.lang.Linker: a var's value is an invokedynamic
  site, a call to a var is one too, and a var cell, a keyword or a symbol is
  a dynamic constant. Advanced mode (M4) emits the same constants and links
  vars directly.

  A try leaves nothing on the operand stack when it catches, so one in a
  position where the stack isn't empty, an argument say, is compiled as a
  function of no arguments, called where it stands, as Clojure's compiler
  does."
  (:require [clojure.string :as str]
            [replique.analyzer.passes :as passes]
            [replique.analyzer.unit :as u])
  (:import [clojure.lang BigInt]
           [java.lang.classfile ClassBuilder ClassFile ClassFile$ClassHierarchyResolverOption
            ClassFile$Option ClassHierarchyResolver CodeBuilder Label]
           [java.lang.constant ClassDesc ConstantDesc ConstantDescs DynamicCallSiteDesc
            DynamicConstantDesc MethodHandleDesc MethodTypeDesc]
           [java.math BigDecimal BigInteger]
           [java.time Instant]
           [java.util UUID]
           [java.util.function Consumer]
           [java.util.regex Pattern]))

;; --- descriptors -----------------------------------------------------------

(defn- cd ^ClassDesc [^String n] (ClassDesc/of n))

(def ^:private ^ClassDesc object ConstantDescs/CD_Object)
(def ^:private ^ClassDesc string ConstantDescs/CD_String)
(def ^:private ^ClassDesc void ConstantDescs/CD_void)
(def ^:private ^ClassDesc cd-int ConstantDescs/CD_int)
(def ^:private ^ClassDesc objects (.arrayType object))
(def ^:private ^ClassDesc afn (cd "replique.lang.AFn"))
(def ^:private ^ClassDesc rest-fn (cd "replique.lang.RestFn"))
(def ^:private ^ClassDesc ifn (cd "replique.lang.IFn"))
(def ^:private ^ClassDesc var-class (cd "replique.lang.Var"))
(def ^:private ^ClassDesc linker (cd "replique.lang.Linker"))
(def ^:private ^ClassDesc keyword-class (cd "replique.lang.Keyword"))
(def ^:private ^ClassDesc symbol-class (cd "replique.lang.Symbol"))
(def ^:private ^ClassDesc ratio-class (cd "replique.lang.Ratio"))
(def ^:private ^ClassDesc util (cd "replique.lang.Util"))
(def ^:private ^ClassDesc namespace-class (cd "replique.lang.Namespace"))
(def ^:private ^ClassDesc reflector (cd "replique.lang.Reflector"))
(def ^:private ^ClassDesc throwable (cd "java.lang.Throwable"))
(def ^:private ^ClassDesc method-handle ConstantDescs/CD_MethodHandle)
(def ^:private ^ClassDesc big-integer (cd "java.math.BigInteger"))
(def ^:private ^ClassDesc big-decimal (cd "java.math.BigDecimal"))
(def ^:private ^ClassDesc hook-class (cd "replique.lang.Hook"))

(defn- mtd ^MethodTypeDesc [^ClassDesc ret & params]
  (MethodTypeDesc/of ret ^"[Ljava.lang.constant.ClassDesc;" (into-array ClassDesc params)))

(defn- generic ^MethodTypeDesc [n] (apply mtd object (repeat n object)))

(defn- class-desc ^ClassDesc [^Class c]
  (ClassDesc/ofDescriptor (.descriptorString c)))

(defn- descs ^"[Ljava.lang.constant.ConstantDesc;" [& xs]
  (into-array ConstantDesc xs))

(def ^:private bsm-value
  (ConstantDescs/ofCallsiteBootstrap linker "value" ConstantDescs/CD_CallSite
                                     (into-array ClassDesc [string string])))
(def ^:private bsm-invoke
  (ConstantDescs/ofCallsiteBootstrap linker "invoke" ConstantDescs/CD_CallSite
                                     (into-array ClassDesc [string string])))
(def ^:private bsm-keyword
  (ConstantDescs/ofConstantBootstrap linker "keyword" keyword-class
                                     (into-array ClassDesc [(.arrayType string)])))
(def ^:private bsm-symbol
  (ConstantDescs/ofConstantBootstrap linker "symbol" symbol-class
                                     (into-array ClassDesc [(.arrayType string)])))
(def ^:private bsm-var
  (ConstantDescs/ofConstantBootstrap linker "var" var-class
                                     (into-array ClassDesc [string string])))

(defn- condy ^DynamicConstantDesc [bsm ^ClassDesc type & args]
  (DynamicConstantDesc/ofNamed bsm "_" type (apply descs args)))

(defn- invoke-const
  "A dynamic constant computed once by calling `handle` on `args`."
  [^ClassDesc type handle & args]
  (apply condy ConstantDescs/BSM_INVOKE type handle args))

;; --- names -----------------------------------------------------------------

(defn- munge-part
  "A namespace segment or a name as part of a JVM binary name, which only
  forbids . ; [ / in a segment: app.core/valid? is the class app.core.valid?"
  [s]
  (let [s (str/replace (str s) #"[.;\[/]" "_")]
    (if (str/blank? s) "_" s)))

(defn package
  "The package of the classes compiled for namespace `ns`."
  [ns]
  (str/join "." (map munge-part (str/split (str ns) #"\."))))

(defn- class-name [ns simple]
  (str (package ns) "." (munge-part simple)))

;; --- errors ----------------------------------------------------------------

(defn- unsupported [node what]
  (u/fail (:span node) :jvm/not-implemented
          (str what " isn't compiled for the JVM yet")))

;; --- emission state --------------------------------------------------------

;; ctx, for the method being emitted:
;;   :ns        the namespace
;;   :this      the ClassDesc of the class
;;   :fields    the ids of the locals the class holds in fields
;;   :self      the id of the fn's own name, which is `this`
;;   :children  a volatile of the plans of the classes this one creates
;;   :slots     {binding-id slot}
;;   :next      a volatile, the next free local slot
;;   :recur     {target-id {:label L :slots [s ...]}}
;;   :clean?    whether the operand stack is empty where the node is emitted
;;   :hook?     whether the form is compiled to be recorded: the hook is the
;;              first element of every class's class data
;;   :outer?    whether the class has the field $outer, the call it was
;;              created in, which a recorded form's functions all have
;;   :call      where the current call is: [:slot s], [:outer], or nil

(defn- alloc! [ctx]
  (let [s @(:next ctx)]
    (vswap! (:next ctx) inc)
    s))

(defn- dirty [ctx] (assoc ctx :clean? false))

(defn- bind [ctx id slot] (assoc-in ctx [:slots id] slot))

(declare emit emit-fn)

;; --- recording -------------------------------------------------------------

(defn- emit-hook [^CodeBuilder cob]
  (.loadConstant cob ^ConstantDesc (condy ConstantDescs/BSM_CLASS_DATA_AT hook-class (Integer/valueOf 0))))

(defn- emit-call
  "Load the current call: what Hook.enter answered for the method, the call
  the function was created in, or null."
  [ctx ^CodeBuilder cob]
  (let [[k s] (:call ctx)]
    (case k
      :slot (.aload cob (int s))
      :outer (do (.aload cob 0)
                 (.getfield cob ^ClassDesc (:this ctx) "$outer" object))
      nil (.aconst_null cob))))

(defn- emit-report
  "Hand the value in `slot` to the hook's value, for site `site`, leaving
  what it answers on the stack."
  [ctx ^CodeBuilder cob site slot]
  (emit-hook cob)
  (.loadConstant cob ^ConstantDesc (Integer/valueOf (int site)))
  (.aload cob (int slot))
  (emit-call ctx cob)
  (.invokeinterface cob hook-class "value" (mtd object cd-int object object)))

(defn- statement [ctx ^CodeBuilder cob node]
  (emit ctx cob node)
  (.pop cob))

;; --- boxing ----------------------------------------------------------------

(def ^:private boxes
  {"J" ["java.lang.Long" "longValue"] "I" ["java.lang.Integer" "intValue"]
   "D" ["java.lang.Double" "doubleValue"] "F" ["java.lang.Float" "floatValue"]
   "S" ["java.lang.Short" "shortValue"] "B" ["java.lang.Byte" "byteValue"]
   "Z" ["java.lang.Boolean" "booleanValue"] "C" ["java.lang.Character" "charValue"]})

(defn- coerce
  "Convert the Object on the stack to `p`, a parameter's type: a number to
  any numeric primitive, as Clojure converts arguments."
  [^CodeBuilder cob ^ClassDesc p]
  (let [d (.descriptorString p)]
    (cond
      (= "Ljava/lang/Object;" d) nil
      (#{"J" "I" "D" "F" "S" "B"} d)
      (do (.checkcast cob (cd "java.lang.Number"))
          (.invokevirtual cob (cd "java.lang.Number") ^String (second (boxes d)) (mtd p)))
      (#{"Z" "C"} d)
      (let [[box getter] (boxes d)]
        (.checkcast cob (cd box))
        (.invokevirtual cob (cd box) ^String getter (mtd p)))
      :else (.checkcast cob p))))

(defn- box
  "Box the value of type `t` on the stack, nil for void."
  [^CodeBuilder cob ^ClassDesc t]
  (let [d (.descriptorString t)]
    (cond
      (= "V" d) (.aconst_null cob)
      (boxes d) (let [b (cd (first (boxes d)))]
                  (.invokestatic cob b "valueOf" (mtd b t)))
      :else nil)))

;; --- constants -------------------------------------------------------------

(def ^:private primitive-types
  {Long/TYPE "java.lang.Long" Integer/TYPE "java.lang.Integer" Double/TYPE "java.lang.Double"
   Float/TYPE "java.lang.Float" Short/TYPE "java.lang.Short" Byte/TYPE "java.lang.Byte"
   Boolean/TYPE "java.lang.Boolean" Character/TYPE "java.lang.Character" Void/TYPE "java.lang.Void"})

(defn- emit-class [^CodeBuilder cob ^Class c]
  (if-let [b (primitive-types c)]
    (.getstatic cob (cd b) "TYPE" (cd "java.lang.Class"))
    (.loadConstant cob ^ConstantDesc (class-desc c))))

(defn- big-integer-const [^BigInteger b]
  (invoke-const big-integer (MethodHandleDesc/ofConstructor big-integer (into-array ClassDesc [string]))
                (str b)))

(defn- names-of [x]
  (if-let [n (namespace x)] [n (name x)] [(name x)]))

(defn- constant-desc
  "The ConstantDesc of a constant `v` that is one, or nil."
  [v]
  (cond
    (string? v) v
    (keyword? v) (apply condy bsm-keyword keyword-class (names-of v))
    (symbol? v) (apply condy bsm-symbol symbol-class (names-of v))
    (instance? BigInt v) (big-integer-const (.toBigInteger ^BigInt v))
    (instance? BigInteger v) (big-integer-const v)
    (instance? BigDecimal v)
    (invoke-const big-decimal (MethodHandleDesc/ofConstructor big-decimal (into-array ClassDesc [string]))
                  (str v))
    (ratio? v)
    (invoke-const ratio-class (MethodHandleDesc/ofConstructor ratio-class (into-array ClassDesc [big-integer big-integer]))
                  (big-integer-const (.numerator ^clojure.lang.Ratio v))
                  (big-integer-const (.denominator ^clojure.lang.Ratio v)))
    (instance? Pattern v)
    (invoke-const (cd "java.util.regex.Pattern")
                  (MethodHandleDesc/ofMethod java.lang.constant.DirectMethodHandleDesc$Kind/STATIC
                                             (cd "java.util.regex.Pattern") "compile"
                                             (mtd (cd "java.util.regex.Pattern") string))
                  (str v))
    (instance? UUID v)
    (invoke-const (cd "java.util.UUID")
                  (MethodHandleDesc/ofMethod java.lang.constant.DirectMethodHandleDesc$Kind/STATIC
                                             (cd "java.util.UUID") "fromString"
                                             (mtd (cd "java.util.UUID") string))
                  (str v))
    (instance? Instant v)
    (invoke-const (cd "java.time.Instant")
                  (MethodHandleDesc/ofMethod java.lang.constant.DirectMethodHandleDesc$Kind/STATIC
                                             (cd "java.time.Instant") "parse"
                                             (mtd (cd "java.time.Instant") (cd "java.lang.CharSequence")))
                  (str v))))

(defn- emit-const [^CodeBuilder cob node v]
  (cond
    (nil? v) (.aconst_null cob)
    (true? v) (.getstatic cob (cd "java.lang.Boolean") "TRUE" (cd "java.lang.Boolean"))
    (false? v) (.getstatic cob (cd "java.lang.Boolean") "FALSE" (cd "java.lang.Boolean"))
    (instance? Long v) (do (.loadConstant cob ^ConstantDesc v) (box cob ConstantDescs/CD_long))
    (instance? Integer v) (do (.loadConstant cob ^ConstantDesc v) (box cob cd-int))
    (instance? Double v) (do (.loadConstant cob ^ConstantDesc v) (box cob ConstantDescs/CD_double))
    (instance? Float v) (do (.loadConstant cob ^ConstantDesc v) (box cob ConstantDescs/CD_float))
    (char? v) (do (.loadConstant cob ^ConstantDesc (Integer/valueOf (int (.charValue ^Character v))))
                  (box cob ConstantDescs/CD_char))
    (class? v) (emit-class cob v)
    :else (if-let [d (constant-desc v)]
            (.loadConstant cob ^ConstantDesc d)
            (if (coll? v)
              (unsupported node "a constant collection, which needs core's collections (M2),")
              (unsupported node (str "a constant of class " (.getName (class v))))))))

;; --- vars ------------------------------------------------------------------

(defn- var-cell [q]
  (condy bsm-var var-class (namespace q) (name q)))

(defn- emit-var-value [^CodeBuilder cob q]
  (.invokedynamic cob (DynamicCallSiteDesc/of bsm-value "value" (mtd object)
                                              (descs (namespace q) (name q)))))

(defn- emit-bind-root
  "Bind the var `q` to the value in `slot`."
  [^CodeBuilder cob q slot]
  (.loadConstant cob ^ConstantDesc (var-cell q))
  (.aload cob (int slot))
  (.invokevirtual cob var-class "bindRoot" (mtd void object)))

;; --- locals ----------------------------------------------------------------

(defn- emit-local [ctx ^CodeBuilder cob id]
  (cond
    (= id (:self ctx)) (.aload cob 0)
    (contains? (:fields ctx) id) (do (.aload cob 0)
                                     (.getfield cob ^ClassDesc (:this ctx) (str "c" id) object))
    :else (if-let [s (get (:slots ctx) id)]
            (.aload cob (int s))
            (throw (IllegalStateException. (str "no slot for local " id))))))

;; --- the special forms -----------------------------------------------------

(defn- truthy [^CodeBuilder cob]
  (.invokestatic cob util "truthy" (mtd ConstantDescs/CD_boolean object)))

(defn- emit-if [ctx ^CodeBuilder cob {:keys [test then else]}]
  (let [else-l (.newLabel cob)
        end (.newLabel cob)]
    (emit ctx cob test)
    (truthy cob)
    (.ifeq cob else-l)
    (emit ctx cob then)
    (.goto_ cob end)
    (.labelBinding cob else-l)
    (emit ctx cob else)
    (.labelBinding cob end)))

(defn- emit-do [ctx cob {:keys [statements ret]}]
  (doseq [s statements] (statement ctx cob s))
  (emit ctx cob ret))

(defn- report-binding
  "Under recording, hand the hook what the binding `b`, a local site, is
  bound to."
  [ctx ^CodeBuilder cob b slot]
  (when-let [site (and (:hook? ctx) (:site b))]
    (emit-report ctx cob site slot)
    (.pop cob)))

(defn- emit-bindings
  "Evaluate each binding's init into a slot of its own, in order; the ctx
  with them in scope, and their slots. `report?`: whether each local site
  reports as it is bound, as a let's do."
  [ctx ^CodeBuilder cob bindings report?]
  (reduce (fn [[ctx slots] b]
            (let [slot (alloc! ctx)]
              (emit ctx cob (:init b))
              (.astore cob (int slot))
              (when report? (report-binding ctx cob b slot))
              [(bind ctx (:id b) slot) (conj slots slot)]))
          [ctx []]
          bindings))

(defn- emit-let [ctx cob {:keys [bindings body]}]
  (let [[ctx _] (emit-bindings ctx cob bindings true)]
    (emit ctx cob body)))

(defn- emit-loop
  "A loop's local sites report at the top of each iteration."
  [ctx ^CodeBuilder cob {:keys [id bindings body]}]
  (let [[ctx slots] (emit-bindings ctx cob bindings false)
        top (.newLabel cob)]
    (.labelBinding cob top)
    (doseq [[b slot] (map vector bindings slots)]
      (report-binding ctx cob b slot))
    (emit (assoc-in ctx [:recur id] {:label top :slots slots}) cob body)))

(defn- emit-recur [ctx ^CodeBuilder cob {:keys [target exprs]}]
  (let [{:keys [label slots]} (get-in ctx [:recur target])]
    (doseq [e exprs] (emit (dirty ctx) cob e))
    (doseq [s (rseq slots)] (.astore cob (int s)))
    (.goto_ cob ^Label label)))

(defn- emit-throw [ctx ^CodeBuilder cob {:keys [exception]}]
  (emit ctx cob exception)
  (.checkcast cob throwable)
  (.athrow cob))

(defn- emit-def [ctx ^CodeBuilder cob {q :var init :init}]
  (if init
    (let [slot (alloc! ctx)]
      (if (= :fn (:op init))
        (emit-fn ctx cob init (str q) (name q))
        (emit ctx cob init))
      (.astore cob (int slot))
      (emit-bind-root cob q slot))
    (do (.loadConstant cob ^ConstantDesc (var-cell q))
        (.pop cob)))
  (.aconst_null cob))

(defn- emit-set! [ctx ^CodeBuilder cob {:keys [target val] :as node}]
  (case (:op target)
    :var (let [slot (alloc! ctx)]
           (emit ctx cob val)
           (.astore cob (int slot))
           (emit-bind-root cob (:var target) slot)
           (.aload cob (int slot)))
    :host-field
    (let [{:keys [static?] member :jvm/member} target]
      (when (= :reflective member) (unsupported node "set! of a field found by reflection"))
      (let [owner (class-desc (:class member))
            type (ClassDesc/ofDescriptor (:descriptor member))
            v (alloc! ctx)]
        (if static?
          (do (emit ctx cob val)
              (.astore cob (int v))
              (.aload cob (int v))
              (coerce cob type)
              (.putstatic cob owner ^String (:name member) type))
          (let [o (alloc! ctx)]
            (emit ctx cob (:target target))
            (.astore cob (int o))
            (emit ctx cob val)
            (.astore cob (int v))
            (.aload cob (int o))
            (.checkcast cob owner)
            (.aload cob (int v))
            (coerce cob type)
            (.putfield cob owner ^String (:name member) type)))
        (.aload cob (int v))))
    (unsupported node "set! of a local field")))

(defn- emit-case [ctx ^CodeBuilder cob {:keys [test nodes default]}]
  (let [v (alloc! ctx)
        end (.newLabel cob)
        labels (mapv (fn [_] (.newLabel cob)) nodes)]
    (emit ctx cob test)
    (.astore cob (int v))
    (doseq [[n l] (map vector nodes labels)
            t (:tests n)]
      (.aload cob (int v))
      (emit-const cob t (:val t))
      (.invokestatic cob util "equiv" (mtd ConstantDescs/CD_boolean object object))
      (.ifne cob ^Label l))
    (if default
      (emit ctx cob default)
      (do (.aload cob (int v))
          (.invokestatic cob util "noMatch" (mtd (cd "java.lang.IllegalArgumentException") object))
          (.athrow cob)))
    (.goto_ cob end)
    (doseq [[n l] (map vector nodes labels)]
      (.labelBinding cob ^Label l)
      (emit ctx cob (:then n))
      (.goto_ cob end))
    (.labelBinding cob end)))

(defn- hoisted
  "`node`, which needs an empty operand stack, as a call of a function of no
  arguments made of it. Its forms report in the call around it, the node
  included, so the node's site stays with the node."
  [node]
  (let [f (passes/mark-closed-over
           {:op :fn :span (:span node) :methods [{:op :fn-method :id nil :params [] :fixed-arity 0
                                                  :variadic? false :body node
                                                  :children [:params :body]}]
            :variadic? false :max-fixed-arity 0 :children [:methods]})]
    {:op :invoke :span (:span node) :fn f :args [] :children [:fn :args]}))

(defn- emit-try [ctx ^CodeBuilder cob {:keys [body catches finally] :as node}]
  (if-not (:clean? ctx)
    (emit ctx cob (hoisted node))
    (let [result (alloc! ctx)
          start (.newLabel cob)
          end (.newLabel cob)
          after (.newLabel cob)
          fin! #(when finally (statement ctx cob finally))]
      (.labelBinding cob start)
      (emit ctx cob body)
      (.astore cob (int result))
      (.labelBinding cob end)
      (fin!)
      (.goto_ cob after)
      (let [ranges (mapv (fn [{:keys [local] :as c}]
                           (let [h (.newLabel cob)
                                 h-end (.newLabel cob)
                                 ex (alloc! ctx)]
                             (.labelBinding cob h)
                             (.astore cob (int ex))
                             (emit (bind ctx (:id local) ex) cob (:body c))
                             (.astore cob (int result))
                             (.labelBinding cob h-end)
                             (fin!)
                             (.goto_ cob after)
                             (if-let [k (:class c)]
                               (.exceptionCatch cob start end h (class-desc (:val k)))
                               (.exceptionCatchAll cob start end h))
                             [h h-end]))
                         catches)]
        (when finally
          (let [h (.newLabel cob)
                ex (alloc! ctx)]
            (.labelBinding cob h)
            (.astore cob (int ex))
            (fin!)
            (.aload cob (int ex))
            (.athrow cob)
            (.exceptionCatchAll cob start end h)
            (doseq [[s e] ranges] (.exceptionCatchAll cob ^Label s ^Label e h)))))
      (.labelBinding cob after)
      (.aload cob (int result)))))

(defn- emit-locking [ctx ^CodeBuilder cob {:keys [lock body] :as node}]
  (if-not (:clean? ctx)
    (emit ctx cob (hoisted node))
    (let [l (alloc! ctx)
          result (alloc! ctx)
          start (.newLabel cob)
          end (.newLabel cob)
          h (.newLabel cob)
          after (.newLabel cob)]
      (emit ctx cob lock)
      (.astore cob (int l))
      (.aload cob (int l))
      (.monitorenter cob)
      (.labelBinding cob start)
      (emit ctx cob body)
      (.astore cob (int result))
      (.aload cob (int l))
      (.monitorexit cob)
      (.labelBinding cob end)
      (.goto_ cob after)
      (.labelBinding cob h)
      (let [ex (alloc! ctx)]
        (.astore cob (int ex))
        (.aload cob (int l))
        (.monitorexit cob)
        (.aload cob (int ex))
        (.athrow cob))
      (.exceptionCatchAll cob start end h)
      (.labelBinding cob after)
      (.aload cob (int result)))))

;; --- calls -----------------------------------------------------------------

(defn- emit-args-array [ctx ^CodeBuilder cob args]
  (.loadConstant cob ^ConstantDesc (Integer/valueOf (count args)))
  (.anewarray cob object)
  (doseq [[i a] (map-indexed vector args)]
    (.dup cob)
    (.loadConstant cob ^ConstantDesc (Integer/valueOf (int i)))
    (emit (dirty ctx) cob a)
    (.aastore cob)))

(def ^:private max-fixed 20)

(defn- emit-invoke [ctx ^CodeBuilder cob {f :fn args :args}]
  (if (= :var (:op f))
    (let [q (:var f)]
      (doseq [a args] (emit (dirty ctx) cob a))
      (.invokedynamic cob (DynamicCallSiteDesc/of bsm-invoke "invoke" (generic (count args))
                                                  (descs (namespace q) (name q)))))
    (do (emit ctx cob f)
        (.checkcast cob ifn)
        (if (<= (count args) max-fixed)
          (do (doseq [a args] (emit (dirty ctx) cob a))
              (.invokeinterface cob ifn "invoke" (generic (count args))))
          (do (doseq [a (take max-fixed args)] (emit (dirty ctx) cob a))
              (emit-args-array ctx cob (drop max-fixed args))
              (.invokeinterface cob ifn "invoke"
                                (apply mtd object (concat (repeat max-fixed object) [objects]))))))))

;; --- interop ---------------------------------------------------------------

(defn- member-type ^MethodTypeDesc [member]
  (MethodTypeDesc/ofDescriptor ^String (:descriptor member)))

(defn- emit-host-call [ctx ^CodeBuilder cob {:keys [target method args static?] member :jvm/member}]
  (if (= :reflective member)
    (if static?
      (do (emit-class cob (:val target))
          (.loadConstant cob ^ConstantDesc (str method))
          (emit-args-array ctx cob args)
          (.invokestatic cob reflector "invokeStatic"
                         (mtd object (cd "java.lang.Class") string objects)))
      (do (emit ctx cob target)
          (.loadConstant cob ^ConstantDesc (str method))
          (emit-args-array ctx cob args)
          (.invokestatic cob reflector "invokeInstance" (mtd object object string objects))))
    (let [mt (member-type member)
          owner (class-desc (:class member))]
      (when-not static?
        (emit ctx cob target)
        (.checkcast cob owner))
      (doseq [[a p] (map vector args (.parameterList mt))]
        (emit (dirty ctx) cob a)
        (coerce cob p))
      (cond
        static? (.invokestatic cob owner ^String (:name member) mt (boolean (:interface? member)))
        (:interface? member) (.invokeinterface cob owner ^String (:name member) mt)
        :else (.invokevirtual cob owner ^String (:name member) mt))
      (box cob (.returnType mt)))))

(defn- emit-host-field [ctx ^CodeBuilder cob {:keys [target field static?] member :jvm/member}]
  (if (= :reflective member)
    (do (emit ctx cob target)
        (.loadConstant cob ^ConstantDesc (str field))
        (.invokestatic cob reflector "field" (mtd object object string)))
    (let [owner (class-desc (:class member))
          type (ClassDesc/ofDescriptor ^String (:descriptor member))]
      (if static?
        (.getstatic cob owner ^String (:name member) type)
        (do (emit ctx cob target)
            (.checkcast cob owner)
            (.getfield cob owner ^String (:name member) type)))
      (box cob type))))

(defn- emit-new [ctx ^CodeBuilder cob {:keys [args] k :class member :jvm/member}]
  (if (= :reflective member)
    (do (emit-class cob (:val k))
        (emit-args-array ctx cob args)
        (.invokestatic cob reflector "construct" (mtd object (cd "java.lang.Class") objects)))
    (let [owner (class-desc (:class member))
          mt (member-type member)]
      (.new_ cob owner)
      (.dup cob)
      (doseq [[a p] (map vector args (.parameterList mt))]
        (emit (dirty ctx) cob a)
        (coerce cob p))
      (.invokespecial cob owner "<init>" mt))))

;; --- dispatch --------------------------------------------------------------

(defn- emit-node [ctx ^CodeBuilder cob node]
  (case (:op node)
    :const (if (= :class (:kind node))
             (emit-class cob (:val node))
             (emit-const cob node (:val node)))
    :quote (emit-const cob node (:val (:expr node)))
    :if (emit-if ctx cob node)
    :do (emit-do ctx cob node)
    :let (emit-let ctx cob node)
    :loop (emit-loop ctx cob node)
    :recur (emit-recur ctx cob node)
    :local (emit-local ctx cob (:id node))
    :var (emit-var-value cob (:var node))
    :the-var (.loadConstant cob ^ConstantDesc (var-cell (:var node)))
    :def (emit-def ctx cob node)
    :set! (emit-set! ctx cob node)
    :fn (emit-fn ctx cob node nil nil)
    :invoke (emit-invoke ctx cob node)
    :throw (emit-throw ctx cob node)
    :try (emit-try ctx cob node)
    :case (emit-case ctx cob node)
    :locking (emit-locking ctx cob node)
    :host-call (emit-host-call ctx cob node)
    :host-field (emit-host-field ctx cob node)
    :new (emit-new ctx cob node)
    :ns (do (.loadConstant cob ^ConstantDesc (str (:name node)))
            (.invokestatic cob namespace-class "findOrCreate" (mtd namespace-class string))
            (.pop cob)
            (.aconst_null cob))
    (:vector :map :set) (unsupported node "a collection literal, which needs core's collections (M2),")
    :with-meta (unsupported node "metadata on a literal")
    :letfn (unsupported node "letfn")
    :qualified-method (unsupported node "a qualified method used as a value")
    (unsupported node (str "the op " (:op node)))))

(defn emit
  "Emit `node`, leaving its value, an Object, on the stack; under recording,
  a value site hands it to the hook first. A try or a locking that has to
  be hoisted (`hoisted`) is the hoisted function's to report."
  [ctx ^CodeBuilder cob node]
  (if-let [site (and (:hook? ctx) (:site node))]
    (if (and (not (:clean? ctx)) (#{:try :locking} (:op node)))
      (emit-node ctx cob (hoisted node))
      (let [slot (alloc! ctx)]
        (emit-node ctx cob node)
        (.astore cob (int slot))
        (emit-report ctx cob site slot)))
    (emit-node ctx cob node)))

;; --- classes ---------------------------------------------------------------

(defn- consumer ^Consumer [f]
  (reify Consumer (accept [_ x] (f x))))

(def ^:private flags-public-final (bit-or ClassFile/ACC_PUBLIC ClassFile/ACC_FINAL ClassFile/ACC_SUPER))

(defn- class-file ^ClassFile [^ClassLoader loader]
  (ClassFile/of (into-array ClassFile$Option
                            [(ClassFile$ClassHierarchyResolverOption/of
                              (.orElse (ClassHierarchyResolver/defaultResolver)
                                       (ClassHierarchyResolver/ofClassLoading loader)))])))

(defn- emit-args
  "An Object[] of the values in `slots`."
  [^CodeBuilder cob slots]
  (.loadConstant cob ^ConstantDesc (Integer/valueOf (count slots)))
  (.anewarray cob object)
  (doseq [[i s] (map-indexed vector slots)]
    (.dup cob)
    (.loadConstant cob ^ConstantDesc (Integer/valueOf (int i)))
    (.aload cob (int s))
    (.aastore cob)))

(defn- method-body
  "Emit the body of `method` in a method whose first `params` slots are
  taken. Under recording, a method that is an enter site tells the hook it
  was entered, and that its call is over in a finally, around everything,
  the loop a recur jumps to included; a method that isn't one reports in
  the call it was created in."
  [ctx ^CodeBuilder cob method param-slots next-slot]
  (let [top (.newLabel cob)
        site (and (:hook? ctx) (:site method))
        ctx (-> ctx
                (assoc :slots (zipmap (map :id (:params method)) param-slots)
                       :next (volatile! next-slot)
                       :clean? true
                       :call (when (:outer? ctx) [:outer]))
                (assoc-in [:recur (:id method)] {:label top :slots param-slots}))]
    (if-not site
      (do (.labelBinding cob top)
          (emit ctx cob (:body method))
          (.areturn cob))
      (let [c (alloc! ctx)
            r (alloc! ctx)
            start (.newLabel cob)
            end (.newLabel cob)
            handler (.newLabel cob)
            exit! (fn []
                    (emit-hook cob)
                    (.aload cob (int c))
                    (.invokeinterface cob hook-class "exit" (mtd void object)))]
        (emit-hook cob)
        (.loadConstant cob ^ConstantDesc (Integer/valueOf (int site)))
        (emit-call ctx cob)
        (emit-args cob param-slots)
        (.invokeinterface cob hook-class "enter" (mtd object cd-int object objects))
        (.astore cob (int c))
        (.labelBinding cob start)
        (.labelBinding cob top)
        (emit (assoc ctx :call [:slot c]) cob (:body method))
        (.astore cob (int r))
        (.labelBinding cob end)
        (exit!)
        (.aload cob (int r))
        (.areturn cob)
        (.labelBinding cob handler)
        (let [ex (alloc! ctx)]
          (.astore cob (int ex))
          (exit!)
          (.aload cob (int ex))
          (.athrow cob))
        (.exceptionCatchAll cob start end handler)))))

(defn- fn-class
  "The plan of the class of the :fn `node`, called `fn-name` in errors and
  `simple` in its class name."
  [ctx node fn-name simple]
  (let [closed (:closed-over node)
        this (cd (class-name (:ns ctx) (or simple "fn")))
        variadic (first (filter :variadic? (:methods node)))
        super (if variadic rest-fn afn)
        children (volatile! [])
        outer? (boolean (and (:hook? ctx) (not (:top? ctx))))
        mctx {:ns (:ns ctx) :loader (:loader ctx) :this this :fields (set closed)
              :self (some-> (:local node) :id) :children children :fn-name fn-name
              :hook? (:hook? ctx) :outer? outer?}
        n (count closed)
        ;; the constructor's parameters: the closed-over values, then $outer
        k (cond-> n outer? inc)
        bytes
        (.build (class-file (:loader ctx)) this
                (consumer
                 (fn [^ClassBuilder clb]
                   (.withFlags clb (int flags-public-final))
                   (.withSuperclass clb super)
                   (doseq [f (cond-> (mapv #(str "c" %) closed) outer? (conj "$outer"))]
                     (.withField clb ^String f object (int (bit-or ClassFile/ACC_PRIVATE ClassFile/ACC_FINAL))))
                   (.withMethodBody clb "<init>" (apply mtd void (repeat k object)) (int ClassFile/ACC_PUBLIC)
                                    (consumer
                                     (fn [^CodeBuilder cob]
                                       (.aload cob 0)
                                       (.invokespecial cob super "<init>" (mtd void))
                                       (doseq [[i f] (map-indexed vector (cond-> (mapv #(str "c" %) closed)
                                                                          outer? (conj "$outer")))]
                                         (.aload cob 0)
                                         (.aload cob (int (inc i)))
                                         (.putfield cob this ^String f object))
                                       (.return_ cob))))
                   (.withMethodBody clb "fnName" (mtd string) (int ClassFile/ACC_PUBLIC)
                                    (consumer
                                     (fn [^CodeBuilder cob]
                                       (.loadConstant cob ^ConstantDesc fn-name)
                                       (.areturn cob))))
                   (doseq [m (:methods node)]
                     (when (and (not (:variadic? m)) (> (count (:params m)) max-fixed))
                       (unsupported m (str "a fn with more than " max-fixed " parameters")))
                     (if (:variadic? m)
                       (let [fixed (:fixed-arity m)]
                         (.withMethodBody clb "requiredArity" (mtd cd-int) (int ClassFile/ACC_PUBLIC)
                                          (consumer
                                           (fn [^CodeBuilder cob]
                                             (.loadConstant cob ^ConstantDesc (Integer/valueOf (int fixed)))
                                             (.ireturn cob))))
                         (.withMethodBody clb "doInvoke" (mtd object objects) (int ClassFile/ACC_PUBLIC)
                                          (consumer
                                           (fn [^CodeBuilder cob]
                                             ;; slot 1 holds every argument: the fixed
                                             ;; ones go to slots 2..., the rest after
                                             (dotimes [i fixed]
                                               (.aload cob 1)
                                               (.loadConstant cob ^ConstantDesc (Integer/valueOf (int i)))
                                               (.aaload cob)
                                               (.astore cob (int (+ 2 i))))
                                             (.aload cob 1)
                                             (.loadConstant cob ^ConstantDesc (Integer/valueOf (int fixed)))
                                             (.invokestatic cob rest-fn "rest" (mtd objects objects cd-int))
                                             (.astore cob (int (+ 2 fixed)))
                                             (method-body mctx cob m (vec (range 2 (+ 3 fixed))) (+ 3 fixed))))))
                       (let [k (count (:params m))]
                         (.withMethodBody clb "invoke" (generic k) (int ClassFile/ACC_PUBLIC)
                                          (consumer
                                           (fn [^CodeBuilder cob]
                                             (method-body mctx cob m (vec (range 1 (inc k))) (inc k)))))))))))]
    {:name (class-name (:ns ctx) (or simple "fn")) :bytes bytes :fields k :children @children}))

(defn- emit-fn
  "Create the function `node`: its class becomes a child of the class being
  emitted, constructed through the MethodHandle at its index in the class
  data, with the values of the locals it closes over, and under recording
  the current call. The hook is before the constructors there."
  [ctx ^CodeBuilder cob node fn-name simple]
  (let [self (some-> (:local node) :name)
        simple (or simple (some-> self str))
        fn-name (or fn-name (str (:ns ctx) "/" (or simple "fn")))
        plan (fn-class ctx node fn-name simple)
        idx (cond-> (count @(:children ctx)) (:hook? ctx) inc)]
    (vswap! (:children ctx) conj plan)
    (.loadConstant cob ^ConstantDesc (condy ConstantDescs/BSM_CLASS_DATA_AT method-handle (Integer/valueOf (int idx))))
    (doseq [id (:closed-over node)]
      (emit-local ctx cob id))
    (when (:hook? ctx) (emit-call ctx cob))
    (.invokevirtual cob method-handle "invokeExact" (generic (:fields plan)))))

(defn form-class
  "The plan of the class of a top-level form, `node`, compiled in namespace
  `ns`: a function of no arguments whose body is the form. `loader` is
  where the classes the code names are found, for the stack maps.

  `hook?`: the form is compiled to be recorded, its sites marked
  (replique.analyzer.sites), and every class of the plan is defined with
  the Hook first in its class data."
  ([ns loader node] (form-class ns loader node false))
  ([ns ^ClassLoader loader node hook?]
   (fn-class {:ns ns :loader loader :hook? hook? :top? true}
             {:op :fn :methods [{:op :fn-method :id nil :params [] :fixed-arity 0 :variadic? false
                                 :body node :span (:span node)}]
              :variadic? false :max-fixed-arity 0 :closed-over []}
             (str ns "/eval")
             "eval")))

(defn anchor-class
  "The bytes of the ordinary class that hosts the hidden classes of the
  package `pkg`: its only member, lookup(), answers a lookup with full
  privilege in the package, which defining a hidden class needs."
  [^String pkg]
  (let [lookup-desc (cd "java.lang.invoke.MethodHandles$Lookup")]
    (.build (ClassFile/of) (cd (str pkg ".$ns"))
            (consumer
             (fn [^ClassBuilder clb]
               (.withFlags clb (int flags-public-final))
               (.withMethodBody clb "lookup" (mtd lookup-desc)
                                (int (bit-or ClassFile/ACC_PUBLIC ClassFile/ACC_STATIC))
                                (consumer
                                 (fn [^CodeBuilder cob]
                                   (.invokestatic cob (cd "java.lang.invoke.MethodHandles") "lookup"
                                                  (mtd lookup-desc))
                                   (.areturn cob)))))))))
