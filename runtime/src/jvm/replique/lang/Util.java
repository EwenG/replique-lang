package replique.lang;

/** What compiled code and the kernel's functions share. */
public final class Util {
    private Util() {}

    /** The test of {@code if}: everything but nil and false is true. */
    public static boolean truthy(Object x) {
        return x != null && x != Boolean.FALSE;
    }

    /** {@code =} on what the kernel knows: numbers by {@link Numbers#equal}, the rest by equals. */
    public static boolean equiv(Object a, Object b) {
        if (a == b) return true;
        if (a == null || b == null) return false;
        if (a instanceof Number x && b instanceof Number y) return Numbers.equal(x, y);
        return a.equals(b);
    }

    /** What a case with no matching clause and no default throws. */
    public static IllegalArgumentException noMatch(Object value) {
        return new IllegalArgumentException("No matching clause: " + Printer.print(value));
    }

    /** {@code (str x)}: nil is the empty string. */
    public static String str(Object x) {
        return x == null ? "" : x.toString();
    }
}
