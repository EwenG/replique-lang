package replique.lang;

/**
 * Where dev mode defines the classes the compiler generates
 * (doc/compilation.md §2.1). Dev mode only: advanced-mode code is loaded as
 * ordinary classes, by an ordinary class loader.
 *
 * <p>The compiler defines one ordinary class per namespace here, the host
 * whose lookup defines that namespace's hidden classes, so that they are in a
 * package named after the namespace. Everything else it generates (each
 * function, each top-level form) is a hidden class, which is unloaded once
 * nothing refers to it: redefining a function doesn't accumulate classes.
 */
public final class Loader extends ClassLoader {
    static {
        registerAsParallelCapable();
    }

    public Loader(ClassLoader parent) {
        super("replique", parent);
    }

    public Class<?> define(String name, byte[] bytes) {
        return defineClass(name, bytes, 0, bytes.length);
    }
}
