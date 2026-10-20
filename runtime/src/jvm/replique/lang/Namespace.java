package replique.lang;

import java.util.concurrent.ConcurrentHashMap;

/**
 * A namespace in dev mode: its name and its var cells. The registry exists in
 * dev mode only; advanced-mode code has no namespaces at run time
 * (doc/compilation.md §3.4). What a namespace refers to and imports is the
 * compiler's business, in the compiler environment, not here.
 */
public final class Namespace {
    private static final ConcurrentHashMap<String, Namespace> NAMESPACES = new ConcurrentHashMap<>();

    public final String name;
    private final ConcurrentHashMap<String, Var> vars = new ConcurrentHashMap<>();

    private Namespace(String name) {
        this.name = name;
    }

    public static Namespace findOrCreate(String name) {
        return NAMESPACES.computeIfAbsent(name, Namespace::new);
    }

    public static Namespace find(String name) {
        return NAMESPACES.get(name);
    }

    /**
     * Forget the namespace `name` and its vars, for tools that start a
     * namespace afresh (the conformance harness runs each file in one). Code
     * already linked to its vars keeps the old cells.
     */
    public static void remove(String name) {
        NAMESPACES.remove(name);
    }

    /** The cell of `name` in this namespace, made unbound if there isn't one. */
    public Var intern(String name) {
        return vars.computeIfAbsent(name, n -> new Var(this, n));
    }

    public Var findVar(String name) {
        return vars.get(name);
    }

    @Override
    public String toString() {
        return name;
    }
}
