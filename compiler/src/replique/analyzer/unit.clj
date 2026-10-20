(ns replique.analyzer.unit
  "The unit of analysis is the top-level form (doc/ir.md §1). While one is
  analysed, its unit collects what the record of the form will hold: the ids
  allocated, the expansion table, the inputs the form consumed, what it
  defines, and its warnings. A unit belongs to one analysis on one thread.")

(defn create
  "`gensym-counter` is the caller's, shared with the reader, so that the
  gensyms of macros and of x# stay unique across the forms of a file."
  [{:keys [cenv host ns mode file gensym-counter]}]
  {:cenv cenv
   :host host
   :ns ns
   :mode (or mode :dev)
   :file file
   :gensym-counter (or gensym-counter (atom 0))
   :ids (volatile! 0)
   :expansions (volatile! {})
   :inputs (volatile! {:vars #{} :macros #{} :files #{} :env #{}})
   :defines (volatile! [])
   :warnings (volatile! [])})

(defn next-id!
  "The next id of the form: bindings, loops, methods and expansions share one
  counter, so ids are allocated in the order the analyzer meets them."
  [u]
  (vswap! (:ids u) inc))

(defn add-expansion! [u id entry]
  (vswap! (:expansions u) assoc id entry))

(defn input!
  "Record that the form consumed `x`; `kind` is :vars, :macros, :files or :env."
  [u kind x]
  (vswap! (:inputs u) update kind conj x))

(defn define! [u q]
  (vswap! (:defines u) conj q))

(defn warn! [u span code message]
  (vswap! (:warnings u) conj {:severity :warning :code code :message message :span span}))

(defn fail
  "Abandon the top-level form with an error at `span`."
  [span code message & {:as more}]
  (throw (ex-info message {::diagnostic (merge {:severity :error :code code
                                                :message message :span span}
                                               more)})))

(defn diagnostic
  "The diagnostic an exception thrown by `fail` carries, or nil."
  [e]
  (::diagnostic (ex-data e)))
