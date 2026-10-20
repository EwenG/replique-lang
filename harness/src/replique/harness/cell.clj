(ns replique.harness.cell
  "The interface every cell implements (doc/conformance.md §4, §7).

  A cell is one way of running cases: one of Replique's four (JVM or JS, dev or
  advanced), or an oracle (§6). The runner hands it a whole file at a time,
  with reader conditionals already resolved for the cell's target, because the
  advanced cells compile a file as one program.

  What a cell returns for each step, keyed by the step's position in its case:

    {:result :value         :value v}               the expression returned v
    {:result :thrown        :category c :data d :message m}
    {:result :compile-error :code c :message m}

  `v` and `d` are JVM data: a cell for a JS target reads its results back
  before returning them. Form steps (inside a scenario) return the same shapes;
  only a failure of one matters to the runner.

  A cell that can't run a case at all (its back end isn't built, say) leaves it
  out, and the runner reports it as skipped.")

(defprotocol Cell
  (-info [cell]
    "{:id :oracle-clojure :target :jvm :mode :dev :oracle? true}")
  (-run-file [cell file cases]
    "Run `cases` (all from `file`, a map from replique.harness.cases/read-file,
    in order) and return {case-id [step-result ...]}. Each vector has one entry
    per step, up to and including the first step that failed to run, if any.")
  (-close [cell]
    "Release whatever the cell holds: a runtime process, a temporary directory."))

(def categories
  "The host-neutral exception categories of expect-throws."
  #{:ex-info :arity :no-match :host})

(defn category
  "The category of an exception, from its class name and message. The same
  rules serve both hosts, since a JS cell reports the class name and message of
  what it caught."
  [class-name message]
  (cond
    (#{"clojure.lang.ExceptionInfo" "replique.lang.ExceptionInfo" "cljs.core/ExceptionInfo"
      "ExceptionInfo"} class-name)
    :ex-info
    (#{"clojure.lang.ArityException" "replique.lang.ArityException" "ArityException"} class-name)
    :arity
    (and message (re-find #"^No matching clause" message)) :no-match
    :else :host))
