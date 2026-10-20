package replique.lang;

/**
 * What code compiled to be recorded reports to (doc/recording.md). The
 * compiler numbers the sites of a top-level form; the code calls the hook
 * the form was run with, the same protocol as in JS.
 *
 * <p>A call is what {@link #enter} answers, and is handed back to
 * {@link #value} for the forms evaluated in it and to {@link #exit} when it
 * is over, by a return or a throw: the compiled code closes it in a
 * {@code finally}. Outside any call, the call is null.
 */
public interface Hook {
    /**
     * A fn method the source wrote is entered. {@code outer} is the call of
     * the fn it was made in, lexically, or null; {@code args} the value of
     * each parameter, the rest one last.
     */
    Object enter(int site, Object outer, Object[] args);

    /** A form has its value, or a local is bound; answers the value. */
    Object value(int site, Object v, Object call);

    /** The call {@code call} is over. */
    void exit(Object call);
}
