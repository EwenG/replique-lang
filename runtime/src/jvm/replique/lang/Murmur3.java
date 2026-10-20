package replique.lang;

/**
 * The hashes of doc/runtime.md §2 that both targets compute: Murmur3 over a
 * string's UTF-16 code units, and the combination Clojure uses for symbols.
 * runtime.js computes the same numbers, so a keyword hashes alike on the JVM
 * and in JS, and alike in Clojure.
 */
public final class Murmur3 {
    private static final int C1 = 0xcc9e2d51;
    private static final int C2 = 0x1b873593;

    private Murmur3() {}

    public static int hashUnencodedChars(CharSequence s) {
        int h1 = 0;
        int n = s.length();
        for (int i = 1; i < n; i += 2) {
            int k1 = s.charAt(i - 1) | (s.charAt(i) << 16);
            h1 = mixH1(h1, mixK1(k1));
        }
        if ((n & 1) == 1) {
            h1 ^= mixK1(s.charAt(n - 1));
        }
        return fmix(h1, 2 * n);
    }

    /** Clojure's Util.hashCombine. */
    public static int hashCombine(int seed, int hash) {
        return seed ^ (hash + 0x9e3779b9 + (seed << 6) + (seed >> 2));
    }

    private static int mixK1(int k1) {
        k1 *= C1;
        k1 = Integer.rotateLeft(k1, 15);
        return k1 * C2;
    }

    private static int mixH1(int h1, int k1) {
        h1 ^= k1;
        h1 = Integer.rotateLeft(h1, 13);
        return h1 * 5 + 0xe6546b64;
    }

    private static int fmix(int h1, int length) {
        h1 ^= length;
        h1 ^= h1 >>> 16;
        h1 *= 0x85ebca6b;
        h1 ^= h1 >>> 13;
        h1 *= 0xc2b2ae35;
        h1 ^= h1 >>> 16;
        return h1;
    }
}
