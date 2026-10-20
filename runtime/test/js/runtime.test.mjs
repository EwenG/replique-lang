// node --test runtime/test/js: the JS kernel. The hashes and the printed text
// are checked against the JVM kernel by runtime/test/replique/lang/kernel_test.clj.

import { test } from "node:test";
import assert from "node:assert/strict";
import * as rt from "../../src/js/replique/runtime.js";

test("keywords are interned", () => {
  assert.equal(rt.keyword("a", "b"), rt.keyword("a", "b"));
  assert.equal(rt.keyword(null, "b"), rt.keyword(null, "b"));
  assert.notEqual(rt.keyword(null, "b"), rt.keyword("a", "b"));
  assert.equal(rt.keyword("a", "b").fqn, "a/b");
});

test("a keyword constant keeps the hash the compiler gave it", () => {
  assert.equal(rt.keyword("given", "hash", 42).hash, 42);
});

test("symbols are values", () => {
  const s = rt.symbol("a", "b");
  assert.ok(s.equals(rt.symbol("a", "b")));
  assert.ok(s.withMeta({}).equals(s));
  assert.ok(!s.equals(rt.symbol(null, "b")));
  assert.equal(s.toString(), "a/b");
});

test("an arity error names the function and the count", () => {
  assert.throws(() => rt.arity(3, "app.core/f"), {
    name: "ArityException",
    message: "Wrong number of args (3) passed to: app.core/f",
  });
});

test("an empty rest parameter is null", () => {
  assert.equal(rt.restArgs([]), null);
  assert.deepEqual(rt.restArgs([1, 2]), [1, 2]);
});

test("a declared var throws until it is defined, then reads its value", () => {
  const ns = rt.namespace("test.declare");
  rt.declare(ns, "w", "w");
  assert.throws(() => ns.w, { message: "Var test.declare/w is unbound" });
  rt.define(ns, "w", 4);
  assert.equal(ns.w, 4);
  rt.declare(ns, "w", "w");
  assert.equal(ns.w, 4, "declaring a defined var leaves it");
  rt.define(ns, "w", 5);
  assert.equal(ns.w, 5);
});

test("a namespace is one object, and a var reads through it", () => {
  const ns = rt.namespace("test.var");
  assert.equal(rt.namespace("test.var"), ns);
  assert.equal(rt.findNamespace("test.var"), ns);
  assert.equal(rt.findNamespace("test.none"), null);
  const v = new rt.Var(ns, "x_SINGLEQUOTE_", "x'");
  rt.define(ns, "x_SINGLEQUOTE_", 1);
  assert.equal(v.deref(), 1);
  rt.define(ns, "x_SINGLEQUOTE_", 2);
  assert.equal(v.deref(), 2);
  assert.equal(rt.print(v), "(var test.var/x')");
});

test("the printer", () => {
  const cases = [
    [null, "nil"],
    [undefined, "nil"],
    [true, "true"],
    [1, "1"],
    [-0, "0"],
    [1.5, "1.5"],
    [2 ** 60, "1152921504606847000.0"],
    [1e21, "1e+21"],
    [1.5e-7, "1.5e-7"],
    [NaN, "##NaN"],
    [Infinity, "##Inf"],
    [-Infinity, "##-Inf"],
    [10n, "10N"],
    ["a\"b\\c\n", '"a\\"b\\\\c\\n"'],
    ["\u0001\u007f", '"\\u0001\\u007f"'],
    ["😀", '"😀"'],
    ["\ud83d", '"\\ud83d"'],
    [rt.keyword("a", "b"), ":a/b"],
    [rt.symbol(null, "x"), "x"],
    [() => 1, "#object[fn]"],
    [[1, "a", [null]], '#array[1 "a" #array[nil]]'],
    [new Date(0), "#object[Date]"],
    [Object.create(null), "#object[Object]"],
  ];
  for (const [x, s] of cases) assert.equal(rt.print(x), s);
});

test("truth: everything but nil and false", () => {
  for (const x of [0, "", [], true, NaN]) assert.equal(rt.truth(x), true);
  for (const x of [null, undefined, false]) assert.equal(rt.truth(x), false);
});

test("equiv", () => {
  assert.ok(rt.equiv(1, 1.0));
  assert.ok(!rt.equiv(1, 1n), "a bigint and a number are different categories");
  assert.ok(rt.equiv(null, undefined));
  assert.ok(rt.equiv(rt.symbol("a", "b"), rt.symbol("a", "b")));
  assert.ok(rt.equiv(rt.keyword(null, "k"), rt.keyword(null, "k")));
  assert.ok(!rt.equiv(NaN, NaN));
});

test("ex-info carries any data, nil included", () => {
  const e = new rt.ExceptionInfo("boom", 1);
  assert.ok(e instanceof Error);
  assert.equal(e.data, 1);
  assert.equal(new rt.ExceptionInfo("boom", undefined).data, null);
  assert.equal(new rt.ExceptionInfo("boom", null, e).cause, e);
  assert.equal(rt.noMatch(rt.keyword(null, "x")).message, "No matching clause: :x");
});

test("set! binds a declared var, and a removed namespace starts afresh", () => {
  const ns = rt.namespace("test.set");
  rt.declare(ns, "v", "v");
  assert.equal(rt.setVar(ns, "v", 3), 3);
  assert.equal(ns.v, 3);
  rt.removeNamespace("test.set");
  assert.equal(rt.findNamespace("test.set"), null);
  assert.notEqual(rt.namespace("test.set"), ns);
});

test("the kernel's functions", () => {
  rt.install();
  const c = rt.namespace("replique.core");
  assert.equal(c.$PLUS$(), 0);
  assert.equal(c.$PLUS$(1n, 2n), 3n);
  assert.equal(c.$SLASH$(1, 2), 0.5);
  assert.equal(c._(5), -5);
  assert.equal(c.$LT$$EQ$(1, 1, 2), true);
  assert.equal(c.str(null, 1.0, rt.keyword(null, "a")), "1:a");
  assert.equal(c.pr_str("a", 1), '"a" 1');
  assert.throws(() => c.$PLUS$(1, "a"), TypeError);
  assert.throws(() => c.inc(), { name: "ArityException", message: "Wrong number of args (0) passed to: replique.core/inc" });
});
