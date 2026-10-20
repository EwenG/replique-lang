package replique.lang;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.concurrent.ConcurrentHashMap;

/**
 * A keyword: interned, so two keywords are equal only if they are the same
 * object. The table holds keywords weakly, as Clojure's does, so keywords
 * made from data (parsed JSON, say) don't accumulate; a keyword the compiled
 * code uses is a constant and stays.
 *
 * <p>The hash is Clojure's: the symbol's plus a constant.
 */
public final class Keyword {
    public final String ns;
    public final String name;
    /** ns/name, or name: what a JS case switches on (doc/ir.md §4). */
    public final String fqn;
    private final int hash;

    private static final ConcurrentHashMap<String, Ref> TABLE = new ConcurrentHashMap<>();
    private static final ReferenceQueue<Keyword> QUEUE = new ReferenceQueue<>();

    private static final class Ref extends WeakReference<Keyword> {
        final String fqn;

        Ref(Keyword k) {
            super(k, QUEUE);
            this.fqn = k.fqn;
        }
    }

    private Keyword(String ns, String name, String fqn) {
        this.ns = ns;
        this.name = name;
        this.fqn = fqn;
        this.hash = Symbol.of(ns, name).hashCode() + 0x9e3779b9;
    }

    public static Keyword intern(String ns, String name) {
        if (name == null) throw new IllegalArgumentException("a keyword needs a name");
        String fqn = ns == null ? name : ns + "/" + name;
        Ref ref = TABLE.get(fqn);
        Keyword k = ref == null ? null : ref.get();
        if (k != null) return k;
        expunge();
        Keyword fresh = new Keyword(ns, name, fqn);
        while (true) {
            Ref existing = TABLE.putIfAbsent(fqn, new Ref(fresh));
            if (existing == null) return fresh;
            Keyword found = existing.get();
            if (found != null) return found;
            TABLE.remove(fqn, existing);
        }
    }

    public static Keyword intern(String name) {
        return intern(null, name);
    }

    private static void expunge() {
        for (Object r; (r = QUEUE.poll()) != null; ) {
            Ref ref = (Ref) r;
            TABLE.remove(ref.fqn, ref);
        }
    }

    @Override
    public int hashCode() {
        return hash;
    }

    @Override
    public String toString() {
        return ":" + fqn;
    }
}
