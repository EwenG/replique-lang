(ns replique.jvm.dev
  "Dev mode on the JVM (doc/compilation.md §2.1), in this JVM: each
  top-level form analysed, compiled to hidden classes and run, which is the
  path of the REPL.

    (def rt (runtime))
    (def cenv (replique.analyzer/environment))
    (eval-form rt cenv '(defn f [] 1) {:ns 'app.core})
    ;; => {:record r :value nil :ns app.core}
    (load-source rt cenv \"(ns app.core) (defn f [] 1) (f)\" {:file \"app/core.rpq\"})
    (eval-form rt cenv '(f) {:ns 'app.core :record {:site f :hook h}})

  A form evaluated with :record is compiled to be recorded
  (doc/recording.md): `:site` chooses its sites, as
  replique.analyzer.sites/mark's, and `h`, a replique.lang.Hook, is what
  its code reports to.

  A runtime owns the class loader the code is defined in. The namespaces and
  the var cells are the kernel's (replique.lang.Namespace), shared by every
  runtime of the JVM; the compiler environment, `cenv`, is the caller's.

  Each namespace's classes are hidden classes in a package named after it,
  defined through the lookup of an ordinary class of that package, its
  anchor (replique.jvm.emit/anchor-class)."
  (:require [replique.analyzer :as a]
            [replique.analyzer.sites :as sites]
            [replique.analyzer.unit :as u]
            [replique.jvm.emit :as emit])
  (:import [java.lang.invoke MethodHandle MethodHandles$Lookup MethodHandles$Lookup$ClassOption
            MethodType]
           [java.util List]
           [replique.lang Core Hook IFn Loader Var]))

(defn runtime
  "A dev-mode runtime: the class loader its code is defined in, child of
  the one that loaded the kernel, and the anchors of its namespaces. Binds
  the kernel's functions in replique.core."
  []
  (Core/install)
  {:loader (Loader. (.getClassLoader Var))
   :anchors (atom {})})

(defn- anchor-lookup
  "The lookup that defines the hidden classes of namespace `ns`."
  ^MethodHandles$Lookup [{:keys [^Loader loader anchors]} ns]
  (let [pkg (emit/package ns)]
    (or (get @anchors pkg)
        (locking anchors
          (or (get @anchors pkg)
              (let [c (.define loader (str pkg ".$ns") (emit/anchor-class pkg))
                    l (.invoke (.getMethod c "lookup" (make-array Class 0)) nil (object-array 0))]
                (swap! anchors assoc pkg l)
                l))))))

(def ^:private no-options (make-array MethodHandles$Lookup$ClassOption 0))

(defn- constructor
  "The constructor of the class `lookup` is on, taking `n` Objects, as a
  handle of type (Object...)Object, which is what the code that creates a
  function calls it with."
  ^MethodHandle [^MethodHandles$Lookup lookup n]
  (-> (.findConstructor lookup (.lookupClass lookup)
                        (MethodType/methodType Void/TYPE ^"[Ljava.lang.Class;" (into-array Class (repeat n Object))))
      (.asType (MethodType/genericMethodType (int n)))))

(defn- define!
  "Define the class of `plan` and its children, the children first; the
  lookup on the class. With a `hook`, it comes first in every class data."
  ^MethodHandles$Lookup [^MethodHandles$Lookup anchor {:keys [^bytes bytes children]} hook]
  (let [data (cond->> (mapv (fn [c] (constructor (define! anchor c hook) (:fields c))) children)
               hook (into [hook]))]
    (if (seq data)
      (.defineHiddenClassWithClassData anchor bytes (List/copyOf ^java.util.Collection data) true no-options)
      (.defineHiddenClass anchor bytes true no-options))))

(defn compile-node
  "The top-level form `node`, compiled in namespace `ns`, as a function of
  no arguments: the IFn that runs it. With a `hook`, the form is compiled
  to be recorded, its sites already marked."
  (^IFn [rt ns node] (compile-node rt ns node nil))
  (^IFn [rt ns node ^Hook hook]
   (let [plan (emit/form-class ns (:loader rt) node (some? hook))
         lookup (define! (anchor-lookup rt ns) plan hook)]
     (.invokeWithArguments (constructor lookup 0) ^List []))))

(defn- error-of [record]
  (first (filter #(= :error (:severity %)) (:diagnostics record))))

(defn run-record
  "Compile and run the record of one top-level form, analysed in namespace
  `ns`. {:value v}, or {:error diagnostic} when the back end can't compile
  it. What the code throws is thrown. With `rec`, {:site :hook :locals?},
  the form is compiled to be recorded: its sites are marked first, unless
  there is no :site, the record's being marked already."
  ([rt ns record] (run-record rt ns record nil))
  ([rt ns record {:keys [hook] :as rec}]
   (if-let [e (error-of record)]
     {:error e}
     (let [record (cond-> record (:site rec) (sites/mark rec))
           f (try (compile-node rt ns (:node record) hook)
                  (catch clojure.lang.ExceptionInfo e
                    (or (some->> (u/diagnostic e) (hash-map :error))
                        (throw e))))]
       (if (map? f)
         f
         {:value (.invoke ^IFn f)})))))

(defn eval-form
  "Analyse, compile and run the form `form`, already read, in namespace
  `ns`. Returns {:record r :ns ns-after} with :value, or :error, a
  diagnostic, when it doesn't compile. Options are analyze-top's, and
  :record {:site :hook :locals?} to record it."
  [rt cenv form {:keys [ns record] :as opts}]
  (let [ns (or ns 'user)
        r (a/analyze-top cenv form (assoc opts :ns ns))]
    (merge {:record r :ns (:ns r)} (run-record rt ns r record))))

(defn load-source
  "Read, analyse, compile and run every form of `src`, in order, each before
  the next is read: a form can use what the forms before it defined, macros
  included. Stops at the first form that doesn't compile, or throws, and
  returns {:results [{:record :value} ...] :diagnostics reader-diagnostics},
  the last result with an :error if one didn't compile."
  [rt cenv src opts]
  (let [results (volatile! [])
        ns (volatile! (or (:ns opts) 'user))
        stop (Object.)
        run (fn [r]
              (let [out (assoc (run-record rt @ns r) :record r)]
                (vswap! results conj out)
                (vreset! ns (:ns r))
                (when (:error out) (throw (ex-info "stop" {::stop stop})))))
        analysis (try (a/analyze-source cenv src (assoc opts :on-unit run))
                      (catch clojure.lang.ExceptionInfo e
                        (if (identical? stop (::stop (ex-data e))) nil (throw e))))]
    {:results @results :diagnostics (:diagnostics analysis)}))
