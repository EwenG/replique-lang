package replique.lang;

import java.lang.invoke.CallSite;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodHandles.Lookup;
import java.lang.invoke.MethodType;
import java.lang.invoke.MutableCallSite;
import java.lang.invoke.WrongMethodTypeException;

/**
 * The bootstrap methods dev-mode code links through (doc/compilation.md §2.1).
 *
 * <p>Two invokedynamic call sites, both naming their var by two static
 * arguments, the namespace and the name:
 *
 * <ul>
 *   <li>{@link #value}: the var's value, {@code ()Object} or any type the
 *       value converts to with {@code asType};
 *   <li>{@link #invoke}: a call to the var's value with the site's arguments.
 *       When the value is a function that offers a direct entry
 *       ({@link AFn#linkTo}), the site calls it; otherwise it calls
 *       {@link IFn#invoke}.
 * </ul>
 *
 * <p>Each site links on its first call to the var's current value, guarded by
 * the var's SwitchPoint. Binding a new root invalidates it, and the site
 * relinks on its next call. A site linked while the var is unbound throws
 * until the var is bound, which is what {@code declare} needs.
 *
 * <p>And three constant-dynamic bootstraps, for the constants compiled code
 * hoists: {@link #keyword}, {@link #symbol} and {@link #var}.
 */
public final class Linker {
    private Linker() {}

    private static final int MAX_FIXED = 20;
    private static final MethodHandle[] IFN_INVOKE = new MethodHandle[MAX_FIXED + 1];
    private static final MethodHandle IFN_APPLY;
    private static final MethodHandle RELINK;
    private static final MethodHandle UNBOUND_CALL;
    private static final MethodHandle UNBOUND_VALUE;
    private static final MethodHandle NOT_A_FUNCTION;

    static {
        try {
            Lookup l = MethodHandles.lookup();
            for (int n = 0; n <= MAX_FIXED; n++) {
                IFN_INVOKE[n] = l.findVirtual(IFn.class, "invoke", MethodType.genericMethodType(n));
            }
            IFN_APPLY = l.findVirtual(IFn.class, "applyTo", MethodType.methodType(Object.class, Object[].class));
            RELINK = l.findStatic(Linker.class, "relink",
                    MethodType.methodType(Object.class, VarSite.class, Object[].class));
            UNBOUND_CALL = l.findStatic(Linker.class, "unboundCall",
                    MethodType.methodType(Object.class, Var.class, Object[].class));
            UNBOUND_VALUE = l.findStatic(Linker.class, "unboundValue",
                    MethodType.methodType(Object.class, Var.class));
            NOT_A_FUNCTION = l.findStatic(Linker.class, "notAFunction",
                    MethodType.methodType(Object.class, Var.class, Object.class, Object[].class));
        } catch (ReflectiveOperationException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    // --- invokedynamic -----------------------------------------------------

    public static CallSite value(Lookup lookup, String name, MethodType type, String ns, String var) {
        if (type.parameterCount() != 0) {
            throw new IllegalArgumentException("a var's value takes no arguments: " + type);
        }
        return new VarSite(Var.intern(ns, var), type, false);
    }

    public static CallSite invoke(Lookup lookup, String name, MethodType type, String ns, String var) {
        return new VarSite(Var.intern(ns, var), type, true);
    }

    static final class VarSite extends MutableCallSite {
        final Var var;
        final boolean call;
        final MethodHandle relink;

        VarSite(Var var, MethodType type, boolean call) {
            super(type);
            this.var = var;
            this.call = call;
            this.relink = RELINK.bindTo(this)
                    .asCollector(Object[].class, type.parameterCount())
                    .asType(type);
            setTarget(relink);
        }

        /** Link to the var's current value; the handle, for the call in progress. */
        MethodHandle link() {
            Var.Linkage l = var.linkage();
            MethodHandle target = call ? callTarget(l, type()) : valueTarget(l, type());
            setTarget(l.switchPoint().guardWithTest(target, relink));
            return target;
        }

        private MethodHandle callTarget(Var.Linkage l, MethodType type) {
            int n = type.parameterCount();
            Object v = l.value();
            if (!l.bound()) {
                return UNBOUND_CALL.bindTo(var).asCollector(Object[].class, n).asType(type);
            }
            if (v instanceof AFn f) {
                MethodHandle direct = f.linkTo(type);
                if (direct != null) {
                    try {
                        return direct.asType(type);
                    } catch (WrongMethodTypeException e) {
                        // the entry can't take this site's types: go through invoke
                    }
                }
            }
            if (v instanceof IFn) {
                MethodHandle h = n <= MAX_FIXED
                        ? IFN_INVOKE[n]
                        : IFN_APPLY.asCollector(Object[].class, n);
                return h.bindTo(v).asType(type);
            }
            return MethodHandles.insertArguments(NOT_A_FUNCTION, 0, var, v)
                    .asCollector(Object[].class, n)
                    .asType(type);
        }

        private MethodHandle valueTarget(Var.Linkage l, MethodType type) {
            if (!l.bound()) {
                return UNBOUND_VALUE.bindTo(var).asType(type);
            }
            return MethodHandles.constant(Object.class, l.value()).asType(type);
        }
    }

    private static Object relink(VarSite site, Object[] args) throws Throwable {
        return site.link().invokeWithArguments(args);
    }

    private static Object unboundCall(Var v, Object[] args) {
        throw new IllegalStateException("Attempting to call unbound fn: " + v);
    }

    private static Object unboundValue(Var v) {
        throw v.unbound();
    }

    private static Object notAFunction(Var v, Object value, Object[] args) {
        String kind = value == null ? "nil" : value.getClass().getName();
        throw new ClassCastException(v + " is not a function: its value is " + kind);
    }

    // --- constant-dynamic --------------------------------------------------

    /** A keyword constant: static arguments [name] or [ns name]. */
    public static Keyword keyword(Lookup lookup, String name, Class<?> type, String... parts) {
        return parts.length == 1 ? Keyword.intern(parts[0]) : Keyword.intern(parts[0], parts[1]);
    }

    /** A symbol constant, without metadata: static arguments [name] or [ns name]. */
    public static Symbol symbol(Lookup lookup, String name, Class<?> type, String... parts) {
        return parts.length == 1 ? Symbol.of(parts[0]) : Symbol.of(parts[0], parts[1]);
    }

    /** A var cell, for #'ns/name and for def. */
    public static Var var(Lookup lookup, String name, Class<?> type, String ns, String var) {
        return Var.intern(ns, var);
    }
}
