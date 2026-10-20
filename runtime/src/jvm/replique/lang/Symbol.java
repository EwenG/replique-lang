package replique.lang;

/**
 * A symbol: a namespace (or null) and a name, and metadata. Symbols are
 * values, not interned: two symbols with the same namespace and name are
 * equal whatever their metadata.
 */
public final class Symbol {
    public final String ns;
    public final String name;
    private final Object meta;
    private final int hash;

    private Symbol(String ns, String name, Object meta) {
        if (name == null) throw new IllegalArgumentException("a symbol needs a name");
        this.ns = ns;
        this.name = name;
        this.meta = meta;
        this.hash = Murmur3.hashCombine(Murmur3.hashUnencodedChars(name), ns == null ? 0 : ns.hashCode());
    }

    public static Symbol of(String ns, String name) {
        return new Symbol(ns, name, null);
    }

    public static Symbol of(String name) {
        return new Symbol(null, name, null);
    }

    public Object meta() {
        return meta;
    }

    public Symbol withMeta(Object m) {
        return new Symbol(ns, name, m);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Symbol s)) return false;
        return hash == s.hash && name.equals(s.name) && (ns == null ? s.ns == null : ns.equals(s.ns));
    }

    @Override
    public int hashCode() {
        return hash;
    }

    @Override
    public String toString() {
        return ns == null ? name : ns + "/" + name;
    }
}
