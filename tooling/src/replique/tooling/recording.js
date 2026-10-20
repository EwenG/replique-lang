// What records, in JS (doc/server.md §5): the calls and the values that
// code compiled to be recorded reports, kept in Node, where they live, and
// asked about by the server. replique.tooling.recording keeps the rest, what
// each unit and each site is, and runs this once in each Node it starts, as
// a form of the runtime: it is then globalThis.$repliqueRecorder.
//
// The same model as on the JVM, the same rules of what is kept, ported from
// replique's replique/debug.cljs: see replique.tooling.recording. A unit and
// a site are the numbers the server gave them. Node has one thread, so a
// call says none.
//
// That a unit recorded is told to the server with `tell`, at most every
// tellEvery ms, as {:unit :call :calls}.
//
// The server goes through what was recorded on the numbers of the values: it
// asks what is kept of a call, the numbers, and then for the values it
// shows, printed. The values stay here.
//
// A value a client browses, in a view of the inspector, is sent as a mirror
// of it, a part at a time: what replique.tooling.inspector browses, on the
// JVM. A mirror says where the value is, a root and a path through it, so
// that the parts not sent yet are asked for by it.

(function ($rt, $hook, $tell) {
  // how many calls of a unit made in no other call kept are kept, the last
  // ones; how many calls are kept at most, of all units; how many values of
  // a form are kept in a call, the first ones and as many of the last ones
  const callsKept = 10;
  const callsKeptInAll = 10000;
  const valuesKept = 10;
  const tellEvery = 200;
  // how much of a value said to the server is printed, in chars
  const printedUpTo = 1000;

  // n -> {definition, calls: [c], enters: Set of its enter sites}
  const units = new Map();
  // c -> {unit, at, count, values: Map site -> {times, values, numbers, head},
  //       open, parent: [c, count], children: [[count, c]], until}, oldest
  //       first: until, once it is over, the count of the call that made it
  //       then, made for its next value
  const calls = new Map();
  let callN = 0;
  // the calls entered and not exited yet, the innermost last
  const running = [];
  const telling = new Set();
  // n -> {space, prop, sites, hook, original, wrapper}: of a unit recording
  // the calls of a var only, the namespace object and the property the var
  // is, what it held and what it holds
  const wrapping = new Map();

  // forget the calls `cs`, and the calls they made, and so on
  function forgetCalls(cs) {
    const todo = [...cs];
    while (todo.length > 0) {
      const c = todo.pop();
      const call = calls.get(c);
      if (call === undefined) continue;
      calls.delete(c);
      const unit = units.get(call.unit);
      if (unit !== undefined) unit.calls = unit.calls.filter((k) => k !== c);
      const parent = call.parent && calls.get(call.parent[0]);
      if (parent) parent.children = parent.children.filter(([, k]) => k !== c);
      for (const [, k] of call.children) todo.push(k);
    }
  }

  // a new call of the unit `n`, made in the call `parent` where it is kept
  function newCall(n, parent) {
    const c = ++callN;
    const p = parent == null ? undefined : calls.get(parent);
    calls.set(c, { unit: n, at: Date.now(), count: 0, values: new Map(), open: true,
                   parent: p ? [parent, p.count] : null, children: [] });
    const unit = units.get(n);
    unit.calls.push(c);
    if (p) p.children.push([p.count, c]);
    const madeInNone = unit.calls.filter((k) => {
      const pk = calls.get(k).parent;
      return !(pk && calls.has(pk[0]));
    });
    forgetCalls(madeInNone.slice(0, Math.max(0, madeInNone.length - callsKept)));
    const over = calls.size - callsKeptInAll;
    if (over > 0) {
      const old = [];
      for (const [k, call] of calls) {
        if (old.length >= over) break;
        if (!call.open) old.push(k);
      }
      forgetCalls(old);
    }
    return c;
  }

  // the calls the call `call` made for its value numbered `i`: the next one it
  // had once they were over
  function madeFor(call, i) {
    return call.children.map(([, k]) => k).filter((k) => calls.get(k)?.until === i - 1);
  }

  // the call `c` is over: made for the next value of the call that made it,
  // of the ones made for the same one, the first ones and the last ones are
  // kept
  function ended(c) {
    const call = calls.get(c);
    const p = call?.parent && calls.get(call.parent[0]);
    if (p && call.until == null) {
      call.until = p.count;
      const same = madeFor(p, call.until + 1);
      forgetCalls(same.slice(valuesKept, Math.max(valuesKept, same.length - valuesKept)));
    }
  }

  // keep `v` as what the site `site` evaluated to in the call `c`
  function keepValue(c, site, v) {
    const call = calls.get(c);
    if (call === undefined) return;
    const i = ++call.count;
    let kept = call.values.get(site);
    if (kept === undefined) {
      kept = { times: 0, values: [], numbers: [], head: [] };
      call.values.set(site, kept);
    }
    kept.times++;
    kept.values.push(v);
    kept.numbers.push(i);
    if (kept.values.length > valuesKept) {
      kept.values.shift();
      // the value that is not kept any more, and the calls made for it
      const out = kept.numbers.shift();
      if (!kept.head.some(([k]) => k === out)) forgetCalls(madeFor(call, out));
    }
    if (kept.head.length < valuesKept) kept.head.push([i, v]);
  }

  // tell the server that the unit `n` recorded, once every tellEvery ms at
  // most: a fn called in a loop mustn't cost the editor a message a call
  function changed(n) {
    if (telling.has(n)) return;
    telling.add(n);
    setTimeout(() => {
      telling.delete(n);
      const unit = units.get(n);
      if (unit !== undefined && unit.calls.length > 0) {
        $tell(`{:unit ${n} :call ${unit.calls.at(-1)} :calls ${unit.calls.length}}`);
      }
    }, tellEvery);
  }

  function printedBriefly(x) {
    let s;
    try {
      s = $rt.print(x);
    } catch (e) {
      s = `#<unprintable ${e instanceof Error ? e.message : typeof e}>`;
    }
    return s.length > printedUpTo ? s.slice(0, printedUpTo) + "..." : s;
  }

  // --- mirrors -------------------------------------------------------------

  // how many parts of a value are sent at most with it, and of one thing
  // that holds parts, how many at first; how much of a value that holds none
  // is printed, in chars
  const mirrored = 500;
  const firstEntries = 100;
  const leafUpTo = 2000;

  // what a value that holds parts is, by identity, kept as long as it is
  const ids = new WeakMap();
  let idN = 0;
  function idOf(o) {
    let i = ids.get(o);
    if (i === undefined) {
      i = ++idN;
      ids.set(o, i);
    }
    return i;
  }

  // the value of a site of a call kept, numbered `i`, as {value}, or null
  function keptValue(c, site, i) {
    const kept = calls.get(c)?.values.get(site);
    if (kept === undefined) return null;
    const j = kept.numbers.indexOf(i);
    const e = j >= 0 ? [i, kept.values[j]] : kept.head.find(([k]) => k === i);
    return e === undefined ? null : { value: e[1] };
  }

  // the value `root` says, {value}, or null: ["call", c, site, number] a
  // value kept of a call, ["var", namespace, property] what a var holds
  function rooted(root) {
    if (root[0] === "call") return keptValue(root[1], root[2], root[3]);
    const ns = $rt.findNamespace(root[1]);
    if (ns === null || !Object.hasOwn(ns, root[2])) return null;
    try {
      return { value: ns[root[2]] };
    } catch (e) {
      return null;
    }
  }

  // the parts of `x`: [key, value]s, or null for a value that holds none.
  // An array's by index, an error's message, data and cause, a var's what it
  // holds, "@", and an object's own properties
  function parts(x) {
    if (Array.isArray(x)) return x.map((v, i) => [i, v]);
    if (x instanceof $rt.Var) {
      try {
        return [["@", x.deref()]];
      } catch (e) {
        return [];
      }
    }
    if (x instanceof Error) {
      return [["message", x.message], ["data", x.data], ["cause", x.cause]]
        .filter(([, v]) => v !== undefined && v !== null);
    }
    if (x !== null && typeof x === "object" && !(x instanceof $rt.Keyword) && !(x instanceof $rt.Symbol)) {
      return Object.keys(x).map((k) => [k, x[k]]);
    }
    return null;
  }

  function kindOf(x) {
    if (x === null || x === undefined) return "nil";
    if (typeof x === "boolean") return "boolean";
    if (typeof x === "number" || typeof x === "bigint") return "number";
    if (typeof x === "string") return "string";
    if (typeof x === "function") return "fn";
    if (x instanceof $rt.Keyword) return "keyword";
    if (x instanceof $rt.Symbol) return "symbol";
    if (x instanceof $rt.Var) return "var";
    if (Array.isArray(x)) return "array";
    return "object";
  }

  function typeOf(x) {
    if (x === null || x === undefined) return null;
    if (typeof x === "function") return "fn";
    if (typeof x !== "object") return { boolean: "Boolean", number: "Number", bigint: "BigInt", string: "String" }[typeof x];
    return x.constructor?.name ?? "Object";
  }

  function printedWithin(x, limit) {
    const s = typeof x === "string" && x.length > limit ? x.slice(0, limit) : x;
    let text;
    try {
      text = $rt.print(s);
    } catch (e) {
      return [`#<unprintable ${e instanceof Error ? e.message : typeof e}>`, true];
    }
    return text.length > limit || s !== x ? [text.slice(0, limit), false] : [text, true];
  }

  // `x` as a mirror: {text, whole, kind, type}, printed, but an array; and
  // of a value that holds parts, its id, shape, count, and its parts sent,
  // entries, [key, mirror]s from the one numbered from, null until
  // `mirrors` sends them from what `queue` is given
  function mirror(x, queue) {
    const ps = parts(x);
    // an array is printed on the JVM, from its parts
    const [text, whole] = Array.isArray(x) ? [null, false] : printedWithin(x, leafUpTo);
    const m = { text, whole, kind: kindOf(x), type: typeOf(x) };
    if (ps === null) return m;
    m.id = idOf(x);
    m.shape = Array.isArray(x) ? "seq" : x instanceof $rt.Var ? "ref" : "object";
    m.count = ps.length;
    m.from = 0;
    m.entries = null;
    queue.push([m, ps]);
    return m;
  }

  // the mirrors of `xs`, their parts sent breadth first, as many as
  // `budget`: what is near the top is sent first
  function mirrors(xs, budget, offset, limit) {
    const queue = [];
    const out = xs.map((x) => mirror(x, queue));
    const top = new Set(out);
    let left = budget;
    for (let k = 0; k < queue.length && left > 0; k++) {
      const [m, ps] = queue[k];
      const from = top.has(m) && offset != null ? offset : 0;
      const n = Math.min(top.has(m) && limit != null ? limit : firstEntries, left);
      m.from = from;
      m.entries = ps.slice(from, from + n).map(([key, v]) => [key, mirror(v, queue)]);
      left -= m.entries.length;
    }
    return out;
  }

  // the value at `path`, keys of parts, from the value `root` says: {value},
  // or null where it leads nowhere
  function at(root, path) {
    let found = rooted(root);
    for (const key of path) {
      if (found === null) return null;
      const e = (parts(found.value) ?? []).find(([k]) => k === key);
      found = e === undefined ? null : { value: e[1] };
    }
    return found;
  }

  // what the var of a unit recording its calls only holds instead of `f`,
  // which it calls: a call made while a call recorded runs is recorded with
  // `hook` at the unit's sites [entry, returned, threw], any other only made
  function callsRecorder(hook, [entry, returned, threw], f) {
    return function (...args) {
      if (running.length === 0) return f.apply(this, args);
      const c = hook.enter(entry, null, args);
      try {
        return hook.value(returned, f.apply(this, args), c);
      } catch (e) {
        hook.value(threw, e, c);
        throw e;
      } finally {
        hook.exit(c);
      }
    };
  }

  // why the calls of `f`, what a var holds, can't be recorded, or null: a
  // class is made with new, which what records its calls would call
  function unwrappable(f) {
    if (typeof f !== "function") return "not a function";
    const p = f.prototype;
    if (/^class\b/.test(Function.prototype.toString.call(f)) ||
        (p != null && Object.getOwnPropertyNames(p).some((k) => k !== "constructor")))
      return "a class, made with new";
    return null;
  }

  // what the var at `prop` of the namespace object `space` holds, or
  // undefined
  function held(space, prop) {
    try {
      return space?.[prop];
    } catch (e) {
      return undefined;
    }
  }

  globalThis.$repliqueRecorder = {
    // the hook the code of the unit `n` reports to, the unit recording from
    // then on. A call is {call, own}: the number of the call kept it
    // records in, null once the unit is gone, and whether it is its own,
    // started on entering.
    unit(n, definition) {
      const unit = { definition, calls: [], enters: new Set() };
      units.set(n, unit);
      const live = () => units.get(n) === unit;
      const latest = () => unit.calls.at(-1) ?? newCall(n, running.at(-1));
      return {
        enter(site, outer, args) {
          let call;
          if (outer != null) call = { call: outer.call, own: false };
          else if (!live()) call = { call: null, own: false };
          else if (definition) {
            const c = newCall(n, running.at(-1));
            running.push(c);
            changed(n);
            call = { call: c, own: true };
          } else call = { call: latest(), own: false };
          // what it was called with is what its entry evaluated to
          if (call.call != null && args.length > 0 && live()) {
            unit.enters.add(site);
            keepValue(call.call, site, args.slice());
          }
          return call;
        },
        value(site, v, call) {
          if (live()) {
            const c = call != null ? call.call : latest();
            if (c != null) {
              keepValue(c, site, v);
              changed(n);
            }
          }
          return v;
        },
        exit(call) {
          if (call.own) {
            const i = running.lastIndexOf(call.call);
            if (i >= 0) running.splice(i, 1);
            const kept = calls.get(call.call);
            if (kept !== undefined) kept.open = false;
            ended(call.call);
            if (live()) changed(n);
          }
        },
      };
    },

    // the unit `n` was evaluated: the calls of its evaluation are forgotten
    // when each of the vars it defined, [namespace, property]s, holds a fn,
    // the evaluation of a fn's definition not being one of its calls
    evaluated(n, places) {
      const unit = units.get(n);
      const fn = ([ns, prop]) => {
        try {
          return typeof $rt.findNamespace(ns)?.[prop] === "function";
        } catch (e) {
          return false;
        }
      };
      if (unit !== undefined && places.length > 0 && places.every(fn)) forgetCalls([...unit.calls]);
      return null;
    },

    uninstrument(n) {
      const unit = units.get(n);
      if (unit !== undefined) {
        forgetCalls([...unit.calls]);
        units.delete(n);
      }
      const w = wrapping.get(n);
      if (w !== undefined) {
        wrapping.delete(n);
        if (held(w.space, w.prop) === w.wrapper) $rt.define(w.space, w.prop, w.original);
      }
      return null;
    },

    // record the calls only of the vars `specs` say, each [n, namespace,
    // property, entry, returned, threw], its unit and that unit's sites:
    // [n, why] of the ones that can't be, as JSON
    wrap(specs) {
      const skipped = [];
      for (const [n, ns, prop, ...sites] of specs) {
        const space = $rt.findNamespace(ns);
        const f = held(space, prop);
        const why = unwrappable(f);
        if (why !== null) {
          skipped.push([n, why]);
          continue;
        }
        const hook = this.unit(n, true);
        const wrapper = callsRecorder(hook, sites, f);
        wrapping.set(n, { space, prop, sites, hook, original: f, wrapper });
        $rt.define(space, prop, wrapper);
      }
      return JSON.stringify(skipped);
    },

    // the var of the unit `n`, recording its calls only, was defined again:
    // whether it is wrapped again, holding a fn
    rewrap(n) {
      const w = wrapping.get(n);
      if (w === undefined) return false;
      const f = held(w.space, w.prop);
      if (f === w.wrapper) return true;
      if (unwrappable(f) !== null) return false;
      w.original = f;
      w.wrapper = callsRecorder(w.hook, w.sites, f);
      $rt.define(w.space, w.prop, w.wrapper);
      return true;
    },

    // what is kept of the call `c`, as JSON, or null where it is not kept:
    // {call, unit, at, parent, children, until, calls: [[c, at]] of its
    // unit, sites: [[site, times, head, tail]]}, the numbers of the values
    // kept of each site, the first ones and the last ones, and not the
    // values
    call(c) {
      const call = calls.get(c);
      if (call === undefined) return null;
      const unit = units.get(call.unit);
      return JSON.stringify({
        call: c, unit: call.unit, at: call.at, parent: call.parent, children: call.children,
        until: call.until ?? null,
        calls: (unit?.calls ?? []).map((k) => [k, calls.get(k).at]),
        sites: [...call.values].map(([site, kept]) =>
          [site, kept.times, kept.head.map(([i]) => i), kept.numbers]),
      });
    },

    // the last call of the unit `n`, as `call` says it, or null
    last(n) {
      const c = units.get(n)?.calls.at(-1);
      return c == null ? null : this.call(c);
    },

    // the values of the call `c` numbered so, each [site, number], printed,
    // as JSON: [value, args], args printed for an entry, null for one not
    // kept; null where the call is not kept
    printed(c, pairs) {
      const call = calls.get(c);
      if (call === undefined) return null;
      const entered = units.get(call.unit)?.enters;
      return JSON.stringify(pairs.map(([site, i]) => {
        const kept = call.values.get(site);
        if (kept === undefined) return null;
        const j = kept.numbers.indexOf(i);
        const e = j >= 0 ? [i, kept.values[j]] : kept.head.find(([k]) => k === i);
        if (e === undefined) return null;
        const v = e[1];
        return [printedBriefly(v), entered?.has(site) && Array.isArray(v) ? v.map(printedBriefly) : null];
      }));
    },

    // the calls kept made in none kept, or with `parent`, the ones that call
    // made, as JSON: each {call, unit, at, parent, made, index, of, last},
    // index which of the calls kept of its unit it is, from 1, of how many
    // are, and last, of each site of `sites` it has a value of, [site,
    // number, value, args], printed
    calls(parent, sites) {
      const cs = parent != null
        ? (calls.get(parent)?.children ?? []).map(([, k]) => k).filter((k) => calls.has(k))
        : [...calls.keys()].filter((k) => !(calls.get(k).parent && calls.has(calls.get(k).parent[0])));
      return JSON.stringify(cs.map((k) => {
        const call = calls.get(k);
        const unit = units.get(call.unit);
        const kept = unit?.calls ?? [];
        return {
          call: k, unit: call.unit, at: call.at, parent: call.parent?.[0] ?? null,
          made: call.children.length, index: kept.indexOf(k) + 1, of: kept.length,
          last: sites.filter((site) => call.values.has(site)).map((site) => {
            const e = call.values.get(site);
            const v = e.values.at(-1);
            // what a call recorded by its calls only threw, by its class and
            // message
            const threw = wrapping.get(call.unit)?.sites[2] === site && v instanceof Error;
            return [site, e.numbers.at(-1),
                    threw ? (v.constructor?.name ?? "Error") + (v.message ? `: ${v.message}` : "") : printedBriefly(v),
                    unit?.enters.has(site) && Array.isArray(v) ? v.map(printedBriefly) : null];
          }),
        };
      }));
    },

    // the values `places` say, [root, path]s, as mirrors, as JSON, null for
    // one that leads nowhere: see `mirror`
    mirror(places) {
      const found = places.map(([root, path]) => at(root, path));
      const ms = mirrors(found.filter((f) => f !== null).map((f) => f.value), mirrored);
      let i = 0;
      return JSON.stringify(found.map((f) => (f === null ? null : ms[i++])));
    },

    // of the value at `path` from `root`, the mirror, its parts from the
    // one numbered `offset` sent, `limit` of them at most; null where it
    // leads nowhere
    entries(root, path, offset, limit) {
      const found = at(root, path);
      return JSON.stringify(found === null ? null : mirrors([found.value], mirrored, offset, limit)[0]);
    },

    // the value at `path` from `root` printed, up to `limit` chars: [text,
    // whole], or null where it leads nowhere
    printedWhole(root, path, limit) {
      const found = at(root, path);
      return JSON.stringify(found === null ? null : printedWithin(found.value, limit));
    },

    // of `rows`, [index, refs]s, the first index where `f` is true, called
    // with the values the refs say, each ["call", c, site, number, arg], arg
    // null but of an entry's, or null for :replique/not-recorded, as JSON:
    // {index}, null where there is none, with the message of the first thing
    // it threw, error, where it threw
    firstWhere(f, rows) {
      const unknown = $rt.keyword("replique", "not-recorded");
      let error = null;
      for (const [index, refs] of rows) {
        const args = refs.map((ref) => {
          const found = ref === null ? null : at(ref.slice(0, 4), ref[4] === null ? [] : [ref[4]]);
          return found === null ? unknown : found.value;
        });
        try {
          if ($rt.truth(f(...args))) return JSON.stringify({ index });
        } catch (e) {
          if (error === null) error = e instanceof Error ? e.message : printedBriefly(e);
        }
      }
      return JSON.stringify(error === null ? { index: null } : { index: null, error });
    },
  };
  return null;
})
