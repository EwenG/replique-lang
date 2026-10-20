package replique.lang;

import java.lang.invoke.SwitchPoint;

/**
 * A var cell in dev mode (doc/compilation.md §2.1): a root value and the
 * SwitchPoint that every call site linked to the current value depends on.
 * Binding a new root invalidates the SwitchPoint, so each dependent site
 * relinks on its next call and sees the new value.
 *
 * <p>Reading an unbound var throws, rather than returning a placeholder
 * value as Clojure does. A var's metadata is compile-time only
 * (doc/language.md §2.3): the cell holds none.
 */
public final class Var {
    private static final Object UNBOUND = new Object();

    public final Namespace ns;
    public final String name;

    private volatile Object root = UNBOUND;
    private SwitchPoint switchPoint = new SwitchPoint();

    Var(Namespace ns, String name) {
        this.ns = ns;
        this.name = name;
    }

    public static Var intern(String ns, String name) {
        return Namespace.findOrCreate(ns).intern(name);
    }

    /** The cell, or null when the namespace or the var doesn't exist. */
    public static Var find(String ns, String name) {
        Namespace n = Namespace.find(ns);
        return n == null ? null : n.findVar(name);
    }

    public boolean isBound() {
        return root != UNBOUND;
    }

    public Object deref() {
        Object v = root;
        if (v == UNBOUND) throw unbound();
        return v;
    }

    public void bindRoot(Object value) {
        SwitchPoint old;
        synchronized (this) {
            root = value;
            old = switchPoint;
            switchPoint = new SwitchPoint();
        }
        SwitchPoint.invalidateAll(new SwitchPoint[] {old});
    }

    /** The value and the SwitchPoint that guards it, read together. */
    record Linkage(Object value, boolean bound, SwitchPoint switchPoint) {}

    synchronized Linkage linkage() {
        return new Linkage(root, root != UNBOUND, switchPoint);
    }

    /** The current SwitchPoint, for tests: it is replaced on every bindRoot. */
    public synchronized SwitchPoint switchPoint() {
        return switchPoint;
    }

    IllegalStateException unbound() {
        return new IllegalStateException("Var " + ns.name + "/" + name + " is unbound");
    }

    @Override
    public String toString() {
        return "#'" + ns.name + "/" + name;
    }
}
