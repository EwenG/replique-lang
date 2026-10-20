// The JS host kernel (doc/runtime.md §2): what compiled code needs before any
// Replique code runs. The JVM kernel is the Java of src/jvm/replique/lang;
// the two agree on hashes and on printed text.
//
// The calling convention. A Replique function is a JS function. It checks
// its number of arguments and throws ArityException (arity) when it has no
// arity for it; a function with several arities dispatches on
// arguments.length. A variadic arity takes its rest parameter as a JS rest
// array, and binds it to restArgs(array): null when there are none, as in
// Clojure.
//
// Dev mode. A namespace is an object and its vars are properties
// (doc/compilation.md §2.2), named by the compiler; namespace(name) returns
// it. A declared var that isn't defined yet throws when read, as on the JVM.

// --- hashes ----------------------------------------------------------------

const C1 = 0xcc9e2d51;
const C2 = 0x1b873593;

function mixK1(k1) {
  k1 = Math.imul(k1, C1);
  k1 = (k1 << 15) | (k1 >>> 17);
  return Math.imul(k1, C2);
}

function mixH1(h1, k1) {
  h1 ^= k1;
  h1 = (h1 << 13) | (h1 >>> 19);
  return (Math.imul(h1, 5) + 0xe6546b64) | 0;
}

function fmix(h1, length) {
  h1 ^= length;
  h1 ^= h1 >>> 16;
  h1 = Math.imul(h1, 0x85ebca6b);
  h1 ^= h1 >>> 13;
  h1 = Math.imul(h1, 0xc2b2ae35);
  h1 ^= h1 >>> 16;
  return h1 | 0;
}

/** Murmur3 over the string's UTF-16 code units: Murmur3.hashUnencodedChars. */
export function hashUnencodedChars(s) {
  let h1 = 0;
  const n = s.length;
  for (let i = 1; i < n; i += 2) {
    h1 = mixH1(h1, mixK1(s.charCodeAt(i - 1) | (s.charCodeAt(i) << 16)));
  }
  if ((n & 1) === 1) h1 ^= mixK1(s.charCodeAt(n - 1));
  return fmix(h1, 2 * n);
}

/** Java's String.hashCode. */
export function stringHashCode(s) {
  let h = 0;
  for (let i = 0; i < s.length; i++) h = (Math.imul(31, h) + s.charCodeAt(i)) | 0;
  return h;
}

/** Clojure's Util.hashCombine. */
export function hashCombine(seed, hash) {
  return (seed ^ ((hash + 0x9e3779b9 + (seed << 6) + (seed >> 2)) | 0)) | 0;
}

function symbolHash(ns, name) {
  return hashCombine(hashUnencodedChars(name), ns === null ? 0 : stringHashCode(ns));
}

// --- symbols and keywords --------------------------------------------------

class Sym {
  constructor(ns, name, meta, hash) {
    this.ns = ns;
    this.name = name;
    this.meta = meta;
    this.hash = hash;
  }
  withMeta(meta) {
    return new Sym(this.ns, this.name, meta, this.hash);
  }
  equals(o) {
    return o instanceof Sym && this.hash === o.hash && this.name === o.name && this.ns === o.ns;
  }
  toString() {
    return this.ns === null ? this.name : this.ns + "/" + this.name;
  }
}

/** A symbol, without metadata; `ns` null for none. */
export function symbol(ns, name) {
  return new Sym(ns, name, null, symbolHash(ns, name));
}

export { Sym as Symbol };

export class Keyword {
  constructor(ns, name, fqn, hash) {
    this.ns = ns;
    this.name = name;
    this.fqn = fqn; // what a case on keywords switches on (doc/ir.md §4)
    this.hash = hash;
  }
  toString() {
    return ":" + this.fqn;
  }
}

const keywords = new Map();

/**
 * The keyword ns/name, interned. Compiled constants pass the hash the
 * compiler computed on the JVM; a keyword made at run time computes it.
 */
export function keyword(ns, name, hash) {
  const fqn = ns === null ? name : ns + "/" + name;
  let k = keywords.get(fqn);
  if (k === undefined) {
    k = new Keyword(ns, name, fqn, hash === undefined ? (symbolHash(ns, name) + 0x9e3779b9) | 0 : hash);
    keywords.set(fqn, k);
  }
  return k;
}

// --- functions -------------------------------------------------------------

export class ArityException extends Error {
  constructor(actual, name) {
    super(`Wrong number of args (${actual}) passed to: ${name}`);
    this.name = "ArityException";
    this.actual = actual;
    this.fnName = name;
  }
}

/** Throws the arity error of a call with `actual` arguments to `name`. */
export function arity(actual, name) {
  throw new ArityException(actual, name);
}

/** A variadic arity's rest parameter: the array, or null when it is empty. */
export function restArgs(array) {
  return array.length === 0 ? null : array;
}

// --- namespaces and vars, dev mode -----------------------------------------

const namespaces = new Map();
const nsName = globalThis.Symbol("replique.ns");

/** The namespace object `name`, made empty if there isn't one. */
export function namespace(name) {
  let ns = namespaces.get(name);
  if (ns === undefined) {
    ns = Object.create(null);
    Object.defineProperty(ns, nsName, { value: name });
    namespaces.set(name, ns);
  }
  return ns;
}

export function findNamespace(name) {
  return namespaces.get(name) ?? null;
}

/** (declare x): reading the var throws until it is defined. */
export function declare(ns, prop, name) {
  if (!Object.hasOwn(ns, prop)) {
    Object.defineProperty(ns, prop, {
      get() {
        throw new Error(`Var ${ns[nsName]}/${name} is unbound`);
      },
      configurable: true,
      enumerable: true,
    });
  }
}

/** (def x v), and every redefinition after it. */
export function define(ns, prop, value) {
  Object.defineProperty(ns, prop, { value, writable: true, configurable: true, enumerable: true });
}

/** (set! x v): binds the var, declared or defined, and returns v. */
export function setVar(ns, prop, value) {
  define(ns, prop, value);
  return value;
}

/** Forget namespace `name`, for tools that start one afresh. */
export function removeNamespace(name) {
  namespaces.delete(name);
}

/** What #'ns/name evaluates to in dev mode. */
export class Var {
  constructor(ns, prop, name) {
    this.ns = ns;
    this.prop = prop;
    this.name = name;
  }
  get nsName() {
    return this.ns[nsName];
  }
  deref() {
    return this.ns[this.prop];
  }
  toString() {
    return `#'${this.nsName}/${this.name}`;
  }
}

// --- truth, equality, exceptions ------------------------------------------

/** Everything but nil and false is true. */
export function truth(x) {
  return x != null && x !== false;
}

/**
 * = on what the kernel knows: one JS number type, so (= 1 1.0) is true; a
 * bigint and a number are never equal, as on the JVM numbers of different
 * categories aren't.
 */
export function equiv(a, b) {
  if (a === b) return true;
  if (a == null || b == null) return a == null && b == null;
  if (a instanceof Sym) return a.equals(b);
  if (a instanceof Var) return b instanceof Var && a.ns === b.ns && a.prop === b.prop;
  return false;
}

/** What ex-info makes: a message, data (any value, nil included) and a cause. */
export class ExceptionInfo extends Error {
  constructor(message, data, cause) {
    super(message, cause === undefined || cause === null ? undefined : { cause });
    this.name = "ExceptionInfo";
    this.data = data ?? null;
  }
}

/** The error of a case without a default that nothing matched. */
export function noMatch(x) {
  return new Error("No matching clause: " + print(x));
}

// --- the printer -----------------------------------------------------------

// What the conformance harness reads results with (doc/conformance.md §3),
// the same text as replique.lang.Printer. JS has one number type: a number
// with no fractional part, within ±2^53, prints as an integer, and any other
// prints so that it reads back as a double. A bigint prints as 1N.

/** `x` as text that reads back to the same value. */
export function print(x) {
  const out = [];
  printTo(x, out);
  return out.join("");
}

function printTo(x, out) {
  if (x === null || x === undefined) out.push("nil");
  else if (typeof x === "boolean") out.push(String(x));
  else if (typeof x === "number") out.push(printNumber(x));
  else if (typeof x === "bigint") out.push(`${x}N`);
  else if (typeof x === "string") out.push(printString(x));
  else if (x instanceof Keyword || x instanceof Sym) out.push(x.toString());
  else if (x instanceof Var) out.push(`(var ${x.nsName}/${x.name})`);
  else if (typeof x === "function") out.push("#object[fn]");
  else if (Array.isArray(x)) {
    out.push("#array[");
    x.forEach((e, i) => {
      if (i > 0) out.push(" ");
      printTo(e, out);
    });
    out.push("]");
  } else out.push(`#object[${x.constructor?.name ?? "Object"}]`);
}

function printNumber(x) {
  if (Number.isNaN(x)) return "##NaN";
  if (x === Infinity) return "##Inf";
  if (x === -Infinity) return "##-Inf";
  if (Number.isInteger(x) && Math.abs(x) <= Number.MAX_SAFE_INTEGER) return String(x);
  const s = String(x);
  return /[.e]/.test(s) ? s : s + ".0";
}

const escapes = { '"': '\\"', "\\": "\\\\", "\n": "\\n", "\t": "\\t", "\r": "\\r", "\b": "\\b", "\f": "\\f" };

function isHigh(c) {
  return c >= 0xd800 && c <= 0xdbff;
}

function isLow(c) {
  return c >= 0xdc00 && c <= 0xdfff;
}

function printString(s) {
  let out = '"';
  for (let i = 0; i < s.length; i++) {
    const ch = s[i];
    const c = s.charCodeAt(i);
    if (escapes[ch] !== undefined) out += escapes[ch];
    else if (isHigh(c) && i + 1 < s.length && isLow(s.charCodeAt(i + 1))) out += ch + s[++i];
    else if (c < 0x20 || c === 0x7f || isHigh(c) || isLow(c)) out += "\\u" + c.toString(16).padStart(4, "0");
    else out += ch;
  }
  return out + '"';
}

// --- the kernel's functions ------------------------------------------------

// The functions of replique.core the kernel provides before core exists
// (doc/runtime.md §5), what the M1 cases call: install() defines them in the
// namespace object. The property names are the compiler's spelling of the
// var names (replique.js.names/munge), which a compiler test checks.
//
// Numbers are ClojureScript's: one number type, and bigint for 1N. Unlike
// ClojureScript, arithmetic on a non-number throws instead of
// concatenating or coercing; a bigint and a number don't mix, as in JS.

function num(x) {
  const t = typeof x;
  if (t === "number" || t === "bigint") return x;
  throw new TypeError(`${print(x)} is not a number`);
}

function add(a, b) {
  return typeof a === "number" && typeof b === "number" ? a + b : num(a) + num(b);
}

function subtract(a, b) {
  return typeof a === "number" && typeof b === "number" ? a - b : num(a) - num(b);
}

function multiply(a, b) {
  return typeof a === "number" && typeof b === "number" ? a * b : num(a) * num(b);
}

function divide(a, b) {
  return num(a) / num(b);
}

function one(x) {
  return typeof x === "bigint" ? 1n : 1;
}

function compare(name, test) {
  const f = function (...xs) {
    if (xs.length < 1) arity(xs.length, "replique.core/" + name);
    for (let i = 0; i < xs.length; i++) num(xs[i]);
    for (let i = 1; i < xs.length; i++) if (!test(xs[i - 1], xs[i])) return false;
    return true;
  };
  return f;
}

function fixed(name, n, f) {
  return function (...xs) {
    if (xs.length !== n) arity(xs.length, "replique.core/" + name);
    return f(...xs);
  };
}

/** (str x): "" for nil, the host's text for anything else. */
export function str(x) {
  return x == null ? "" : typeof x === "string" ? x : String(x);
}

function join(xs, readably) {
  return xs.map((x) => (readably || typeof x !== "string" ? print(x) : x)).join(" ");
}

let write =
  typeof process !== "undefined" && process.stdout
    ? (s) => process.stdout.write(s)
    : (() => {
        let line = "";
        return (s) => {
          const parts = (line + s).split("\n");
          line = parts.pop();
          for (const p of parts) console.log(p);
        };
      })();

/** Where print and println write, a function of a string. */
export function setWriter(f) {
  write = f;
}

function out(s) {
  write(s);
  return null;
}

let installed = false;

/** Define the kernel's functions in replique.core. Idempotent. */
export function install() {
  if (installed) return;
  installed = true;
  const core = namespace("replique.core");
  const fns = {
    $PLUS$: (...xs) => (xs.length === 0 ? 0 : xs.length === 1 ? num(xs[0]) : xs.reduce(add)),
    $STAR$: (...xs) => (xs.length === 0 ? 1 : xs.length === 1 ? num(xs[0]) : xs.reduce(multiply)),
    _: (...xs) => {
      if (xs.length < 1) arity(0, "replique.core/-");
      return xs.length === 1 ? -num(xs[0]) : xs.reduce(subtract);
    },
    $SLASH$: (...xs) => {
      if (xs.length < 1) arity(0, "replique.core//");
      return xs.length === 1 ? divide(one(xs[0]), xs[0]) : xs.reduce(divide);
    },
    inc: fixed("inc", 1, (x) => num(x) + one(x)),
    dec: fixed("dec", 1, (x) => num(x) - one(x)),
    $EQ$: (...xs) => {
      if (xs.length < 1) arity(0, "replique.core/=");
      for (let i = 1; i < xs.length; i++) if (!equiv(xs[i - 1], xs[i])) return false;
      return true;
    },
    not$EQ$: (...xs) => {
      if (xs.length < 1) arity(0, "replique.core/not=");
      for (let i = 1; i < xs.length; i++) if (!equiv(xs[i - 1], xs[i])) return true;
      return false;
    },
    $LT$: compare("<", (a, b) => a < b),
    $GT$: compare(">", (a, b) => a > b),
    $LT$$EQ$: compare("<=", (a, b) => a <= b),
    $GT$$EQ$: compare(">=", (a, b) => a >= b),
    not: fixed("not", 1, (x) => !truth(x)),
    nil$QMARK$: fixed("nil?", 1, (x) => x == null),
    some$QMARK$: fixed("some?", 1, (x) => x != null),
    identity: fixed("identity", 1, (x) => x),
    str: (...xs) => xs.map(str).join(""),
    ex_info: function (message, data, cause) {
      if (arguments.length < 2 || arguments.length > 3) arity(arguments.length, "replique.core/ex-info");
      return new ExceptionInfo(message, data, cause);
    },
    ex_data: fixed("ex-data", 1, (e) => (e instanceof ExceptionInfo ? e.data : null)),
    ex_message: fixed("ex-message", 1, (e) => (e instanceof Error ? e.message : null)),
    ex_cause: fixed("ex-cause", 1, (e) => (e instanceof Error ? (e.cause ?? null) : null)),
    instance$QMARK$: fixed("instance?", 2, (c, x) => x instanceof c),
    keyword: function (a, b) {
      if (arguments.length === 2) return keyword(a, b);
      if (arguments.length !== 1) arity(arguments.length, "replique.core/keyword");
      if (a instanceof Keyword) return a;
      const i = a.indexOf("/");
      return i > 0 && i < a.length - 1 ? keyword(a.substring(0, i), a.substring(i + 1)) : keyword(null, a);
    },
    symbol: function (a, b) {
      if (arguments.length === 2) return symbol(a, b);
      if (arguments.length !== 1) arity(arguments.length, "replique.core/symbol");
      if (a instanceof Sym) return a;
      const i = a.indexOf("/");
      return i > 0 && i < a.length - 1 ? symbol(a.substring(0, i), a.substring(i + 1)) : symbol(null, a);
    },
    deref: fixed("deref", 1, (x) => x.deref()),
    count: fixed("count", 1, (x) => {
      if (x == null) return 0;
      if (typeof x === "string" || Array.isArray(x)) return x.length;
      throw new Error(`count not supported on ${print(x)}`);
    }),
    pr_str: (...xs) => join(xs, true),
    prn: (...xs) => out(join(xs, true) + "\n"),
    print: (...xs) => out(join(xs, false)),
    println: (...xs) => out(join(xs, false) + "\n"),
  };
  for (const [prop, f] of Object.entries(fns)) define(core, prop, f);
}
