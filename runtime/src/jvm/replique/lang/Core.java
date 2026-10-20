package replique.lang;

/**
 * The functions of replique.core the kernel provides before core exists
 * (doc/runtime.md §2): what the M1 cases call. {@link #install} binds them
 * as the roots of their vars, in dev mode.
 *
 * <p>The compiler declares more kernel functions than these
 * (replique.analyzer.bootstrap): the ones on collections and seqs arrive
 * with core's collections (M2), and until then their vars are unbound, so
 * calling one throws "Attempting to call unbound fn".
 */
public final class Core {
    private Core() {}

    public static final String NS = "replique.core";

    private abstract static class Fn extends AFn {
        private final String name;

        Fn(String name) {
            this.name = name;
        }

        @Override
        public String fnName() {
            return NS + "/" + name;
        }
    }

    /** A variadic function: every arity at least {@code required} goes to doInvoke. */
    private abstract static class VarFn extends RestFn {
        private final String name;
        private final int required;

        VarFn(String name, int required) {
            this.name = name;
            this.required = required;
        }

        @Override
        public String fnName() {
            return NS + "/" + name;
        }

        @Override
        public int requiredArity() {
            return required;
        }
    }

    private interface Op {
        Object apply(Object a, Object b);
    }

    private interface Test {
        boolean test(Object a, Object b);
    }

    /** (f), (f x), (f x y) and on, folding `op` from the left. */
    private static VarFn fold(String name, Object identity, Op op, boolean unary) {
        return new VarFn(name, identity == null ? 1 : 0) {
            @Override
            public Object invoke(Object a, Object b) {
                return op.apply(a, b);
            }

            @Override
            public Object doInvoke(Object[] args) {
                if (args.length == 0) return identity;
                if (args.length == 1) return unary ? op.apply(identity, args[0]) : (Number) args[0];
                Object acc = args[0];
                for (int i = 1; i < args.length; i++) acc = op.apply(acc, args[i]);
                return acc;
            }
        };
    }

    /** (f x), (f x y) and on: whether `test` holds of each consecutive pair. */
    private static VarFn chain(String name, Test test) {
        return new VarFn(name, 1) {
            @Override
            public Object invoke(Object a, Object b) {
                return test.test(a, b);
            }

            @Override
            public Object doInvoke(Object[] args) {
                for (int i = 1; i < args.length; i++) {
                    if (!test.test(args[i - 1], args[i])) return false;
                }
                return true;
            }
        };
    }

    private static String join(Object[] args, boolean readably) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < args.length; i++) {
            if (i > 0) sb.append(' ');
            Object x = args[i];
            if (!readably && (x instanceof String || x instanceof Character)) sb.append(x);
            else Printer.print(x, sb);
        }
        return sb.toString();
    }

    private static final InheritableThreadLocal<java.io.Writer> OUT = new InheritableThreadLocal<>();

    /**
     * Where prn, print and println write on this thread, and on the threads
     * it starts from now on; null for System.out, the default. A REPL points
     * it at its connection, as Clojure binds *out*.
     */
    public static void setOut(java.io.Writer w) {
        if (w == null) OUT.remove();
        else OUT.set(w);
    }

    private static Object out(String s) {
        java.io.Writer w = OUT.get();
        if (w == null) {
            System.out.print(s);
            System.out.flush();
        } else {
            try {
                w.write(s);
                w.flush();
            } catch (java.io.IOException e) {
                throw new java.io.UncheckedIOException(e);
            }
        }
        return null;
    }

    private static int installed;

    /** Bind the kernel's functions in replique.core. Idempotent. */
    public static synchronized void install() {
        if (installed++ > 0) return;
        def(fold("+", 0L, Numbers::add, false));
        def(fold("*", 1L, Numbers::multiply, false));
        def(new VarFn("-", 1) {
            @Override
            public Object invoke(Object a) {
                return Numbers.negate(a);
            }

            @Override
            public Object invoke(Object a, Object b) {
                return Numbers.subtract(a, b);
            }

            @Override
            public Object doInvoke(Object[] args) {
                if (args.length == 1) return Numbers.negate(args[0]);
                Object acc = args[0];
                for (int i = 1; i < args.length; i++) acc = Numbers.subtract(acc, args[i]);
                return acc;
            }
        });
        def(new VarFn("/", 1) {
            @Override
            public Object invoke(Object a, Object b) {
                return Numbers.divide(a, b);
            }

            @Override
            public Object doInvoke(Object[] args) {
                if (args.length == 1) return Numbers.divide(1L, args[0]);
                Object acc = args[0];
                for (int i = 1; i < args.length; i++) acc = Numbers.divide(acc, args[i]);
                return acc;
            }
        });
        def(new Fn("inc") {
            @Override
            public Object invoke(Object x) {
                return Numbers.inc(x);
            }
        });
        def(new Fn("dec") {
            @Override
            public Object invoke(Object x) {
                return Numbers.dec(x);
            }
        });
        def(chain("=", Util::equiv));
        def(chain("<", Numbers::lt));
        def(chain(">", Numbers::gt));
        def(chain("<=", Numbers::lte));
        def(chain(">=", Numbers::gte));
        def(new VarFn("not=", 1) {
            @Override
            public Object doInvoke(Object[] args) {
                for (int i = 1; i < args.length; i++) {
                    if (!Util.equiv(args[i - 1], args[i])) return true;
                }
                return false;
            }
        });
        def(new Fn("not") {
            @Override
            public Object invoke(Object x) {
                return x == null || x == Boolean.FALSE;
            }
        });
        def(new Fn("nil?") {
            @Override
            public Object invoke(Object x) {
                return x == null;
            }
        });
        def(new Fn("some?") {
            @Override
            public Object invoke(Object x) {
                return x != null;
            }
        });
        def(new Fn("identity") {
            @Override
            public Object invoke(Object x) {
                return x;
            }
        });
        def(new VarFn("str", 0) {
            @Override
            public Object invoke(Object x) {
                return Util.str(x);
            }

            @Override
            public Object doInvoke(Object[] args) {
                StringBuilder sb = new StringBuilder();
                for (Object x : args) sb.append(Util.str(x));
                return sb.toString();
            }
        });
        def(new Fn("ex-info") {
            @Override
            public Object invoke(Object message, Object data) {
                return new ExceptionInfo((String) message, data);
            }

            @Override
            public Object invoke(Object message, Object data, Object cause) {
                return new ExceptionInfo((String) message, data, (Throwable) cause);
            }
        });
        def(new Fn("ex-data") {
            @Override
            public Object invoke(Object e) {
                return e instanceof ExceptionInfo i ? i.data : null;
            }
        });
        def(new Fn("ex-message") {
            @Override
            public Object invoke(Object e) {
                return e instanceof Throwable t ? t.getMessage() : null;
            }
        });
        def(new Fn("ex-cause") {
            @Override
            public Object invoke(Object e) {
                return e instanceof Throwable t ? t.getCause() : null;
            }
        });
        def(new Fn("instance?") {
            @Override
            public Object invoke(Object c, Object x) {
                return ((Class<?>) c).isInstance(x);
            }
        });
        def(new Fn("keyword") {
            @Override
            public Object invoke(Object name) {
                if (name instanceof Keyword) return name;
                String s = (String) name;
                int i = s.indexOf('/');
                return i > 0 && i < s.length() - 1 ? Keyword.intern(s.substring(0, i), s.substring(i + 1)) : Keyword.intern(s);
            }

            @Override
            public Object invoke(Object ns, Object name) {
                return Keyword.intern((String) ns, (String) name);
            }
        });
        def(new Fn("symbol") {
            @Override
            public Object invoke(Object name) {
                if (name instanceof Symbol) return name;
                String s = (String) name;
                int i = s.indexOf('/');
                return i > 0 && i < s.length() - 1 ? Symbol.of(s.substring(0, i), s.substring(i + 1)) : Symbol.of(s);
            }

            @Override
            public Object invoke(Object ns, Object name) {
                return Symbol.of((String) ns, (String) name);
            }
        });
        def(new Fn("deref") {
            @Override
            public Object invoke(Object x) {
                return ((Var) x).deref();
            }
        });
        def(new Fn("count") {
            @Override
            public Object invoke(Object x) {
                if (x == null) return 0L;
                if (x instanceof CharSequence s) return (long) s.length();
                if (x.getClass().isArray()) return (long) java.lang.reflect.Array.getLength(x);
                throw new UnsupportedOperationException("count not supported on " + x.getClass().getName());
            }
        });
        def(new VarFn("pr-str", 0) {
            @Override
            public Object doInvoke(Object[] args) {
                return join(args, true);
            }
        });
        def(new VarFn("prn", 0) {
            @Override
            public Object doInvoke(Object[] args) {
                return out(join(args, true) + "\n");
            }
        });
        def(new VarFn("print", 0) {
            @Override
            public Object doInvoke(Object[] args) {
                return out(join(args, false));
            }
        });
        def(new VarFn("println", 0) {
            @Override
            public Object doInvoke(Object[] args) {
                return out(join(args, false) + "\n");
            }
        });
    }

    private static void def(AFn f) {
        String n = f.fnName();
        Var.intern(NS, n.substring(NS.length() + 1)).bindRoot(f);
    }
}
