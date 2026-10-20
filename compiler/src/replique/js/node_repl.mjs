// The Node side of JS dev mode (replique.js.dev): it dials the compiler's
// JVM back on the port it is given, then runs what it is sent, one request
// at a time. A dev tool, not part of the runtime: runtime.js never assumes a
// REPL is connected (doc/compilation.md §3.4).
//
// A request is one line of JSON, {"code": ..., "file": ..., "hook": ...},
// where code is a function expression of the runtime
// (replique.js.emit/form-js). With a hook, a JS expression evaluated before
// the code runs, the code is a function of the runtime and of the hook it
// reports to, being compiled to be recorded (doc/recording.md). Its third
// argument is `tell`, which sends the JVM a string as an event, then or any
// time later. The answer is one line of tab-separated fields, each printed
// by the kernel's printer, which escapes tabs and newlines:
//
//   out    <string>                          what the form printed, any number
//   value  <value>                           and then one of these two
//   thrown <class name> <message> <ex-data>
//
// and an event is a line of its own, between answers or inside one:
//
//   event  <string>
//
// What is printed while no request runs, a timer firing, goes to stdout.

import * as net from "node:net";
import * as readline from "node:readline";
import * as vm from "node:vm";
import * as rt from "./runtime.js";

rt.install();

function className(e) {
  if (e instanceof rt.ExceptionInfo) return "ExceptionInfo";
  if (e instanceof rt.ArityException) return "ArityException";
  if (e !== null && typeof e === "object" && typeof e.constructor === "function") return e.constructor.name;
  return typeof e;
}

const stdout = (s) => process.stdout.write(s);

const tell = (s) => sock.write("event\t" + rt.print(String(s)) + "\n");

function answer(code, file, hook) {
  rt.setWriter((s) => sock.write("out\t" + rt.print(s) + "\n"));
  try {
    const f = vm.runInThisContext(code, { filename: file });
    const h = hook === undefined ? undefined : vm.runInThisContext(hook);
    return ["value", rt.print(f(rt, h, tell))];
  } catch (e) {
    const message = e instanceof Error ? e.message : rt.print(e);
    return ["thrown", rt.print(className(e)), rt.print(message), rt.print(e instanceof rt.ExceptionInfo ? e.data : null)];
  } finally {
    rt.setWriter(stdout);
  }
}

const sock = net.connect(Number(process.argv[2]), "127.0.0.1");
sock.setEncoding("utf8");
sock.on("connect", () => sock.write("ready\n"));
sock.on("close", () => process.exit(0));
sock.on("error", () => process.exit(1));
readline.createInterface({ input: sock }).on("line", (line) => {
  const { code, file, hook } = JSON.parse(line);
  sock.write(answer(code, file, hook).join("\t") + "\n");
});
