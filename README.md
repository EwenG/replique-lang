# Replique

A Clojure-like language that compiles to JVM bytecode and to JavaScript (ES
modules, browser first). It is the successor of two existing projects and keeps
the best of each:

- [replique-clj](../../clojure/replique-clj): a fork of Clojure 1.12.5, with a
  compiler-integrated analysis subsystem and a plan for rewriting the compiler
  ([`compiler-rewrite.md`](../../clojure/replique-clj/doc/compiler-rewrite.md)).
- [replique-cljs](../../clojure/replique-cljs): a ClojureScript compiler written
  from scratch, with an analyzer that produces an AST, ESM output, injective
  naming, and var metadata kept in the compiler environment.

Status: **M1 done: the special forms in dev mode on the JVM and in JS, with a
REPL server replique.el talks to. Code compiled to be recorded, on both
targets, recorded on both for replique.el's `C-c C-d r`, and gone through
with `C-c C-d v`, its views of values included.** Started 2026-10-10. Advanced mode and core are not written yet.

## What is different from Clojure

The conclusion up front. Each point is argued in `doc/`.

- **Eager.** No lazy data structures. Seqs remain, but as views over realized
  collections; unbounded sources are reducibles. Core is built on transducers.
- **`def` returns nil**, and var metadata exists only at compile time.
  `with-meta` on values is unchanged.
- **Two compilation modes.** Dev mode links every var dynamically
  (`invokedynamic` on the JVM, namespace-object properties in JS) so the REPL can
  redefine anything. Advanced mode links directly, unless a var says otherwise,
  has no `eval`, and ships without the compiler.
- **Dynamic vars are scoped values**, with one semantics on both targets, and
  concurrency is structured by default.
- **`async`/`await` is in the language**; core.async is a library on top, with
  no state-machine macro.
- **Validation is a first-party library** built on general compiler hooks, with
  namespaces as the only schema registry and schema selection.

The price: existing Clojure libraries do not run unchanged. Java and npm
libraries do.

## Documentation

| | |
|---|---|
| [`doc/language.md`](doc/language.md) | the language: laziness, vars, metadata, core, numbers, the other lessons from Clojure |
| [`doc/compilation.md`](doc/compilation.md) | the compiler: one front end, two back ends, dev and advanced modes, linking rules |
| [`doc/reader.md`](doc/reader.md) | the reader: the lossless CST, forms with spans, what differs from Clojure's reader |
| [`doc/analyzer.md`](doc/analyzer.md) | the analyzer: forms to IR, spans and origins, macros, types, interop |
| [`doc/ir.md`](doc/ir.md) | the IR: what every node carries, the type lattice, the node inventory and fields |
| [`doc/jvm.md`](doc/jvm.md) | the JVM back end: classes, hidden classes, values, linking, control |
| [`doc/js.md`](doc/js.md) | the JS back end: a form as a function, names, statements and expressions, the Node side |
| [`doc/server.md`](doc/server.md) | the REPL server: the part of replique's protocol it speaks, the repl, directives, recording |
| [`doc/recording.md`](doc/recording.md) | code compiled to be recorded: the hook, which forms are sites, each back end |
| [`doc/conformance.md`](doc/conformance.md) | the conformance harness: case files, the four cells, comparing results |
| [`doc/runtime.md`](doc/runtime.md) | the host kernel, the protocols the compiler depends on, bootstrap order |
| [`doc/concurrency.md`](doc/concurrency.md) | dynamic vars, structured concurrency, `async`/`await`, core.async |
| [`doc/validation.md`](doc/validation.md) | schemas, the registry, selection, compiler hooks, ideas from other libraries |
| [`doc/roadmap.md`](doc/roadmap.md) | milestones, and the next step |

## Repository

| | |
|---|---|
| `runtime/` | the host kernel and core; never depends on the compiler. So far the kernel's first pieces, in Java and in `runtime.js` ([`doc/runtime.md`](doc/runtime.md) §5) |
| `compiler/` | the compiler, depends on the runtime. So far the reader ([`doc/reader.md`](doc/reader.md)), the analyzer ([`doc/analyzer.md`](doc/analyzer.md)) and the dev back ends, JVM ([`doc/jvm.md`](doc/jvm.md)) and JS ([`doc/js.md`](doc/js.md)) |
| `tooling/` | replique's tooling: the REPL server replique.el talks to, recording, the inspector ([`doc/server.md`](doc/server.md)), depends on the compiler |
| `harness/` | the conformance harness ([`doc/conformance.md`](doc/conformance.md)) |
| `conformance/` | the conformance suite, `.rpq` case files |
| `deps.edn` | the development entry point, on replique-clj |

Needs the Clojure CLI, and Node for the JS back end, the ClojureScript oracle
and the JS kernel's tests (skipped without it). replique-clj and replique-cljs are
expected at `~/clojure/`, beside this directory's parent.

```
clojure -X:deps prep            # once: compiles the runtime's Java
clojure -M:test                 # the runtime's, compiler's, tooling's and harness's tests
clojure -M:server               # the REPL server, for replique.el: M-x replique-connect
clojure -M:conformance          # the suite, on the JVM and JS dev cells and the Clojure oracle
clojure -M:cljs:conformance     # and on the ClojureScript oracle
clojure -M:bench                # the reader's speed against Clojure's reader
```

The baseline is JDK 27. What is built so far runs on JDK 25, which the JVM
back end and the harness use: dev mode runs the code it compiles in the
compiler's own JVM.

## Names

| where | name |
|---|---|
| the language | Replique |
| source files | `.rpq` (not in GitHub Linguist; `.rq` is SPARQL) |
| GitHub organisation | `replique-lang` |
| Maven groupId | `io.github.replique-lang` (needs the GitHub organisation) or `org.replique-lang` (needs the domain) |
| npm | the `@replique-lang` scope (the bare `replique` package belongs to someone else) |
| domain | `replique-lang.dev` or `.org` |

Availability was checked on 2026-10-10, and none of these is reserved yet.
replique-clj and replique-cljs keep their names: they remain a Clojure fork and a
ClojureScript compiler.
