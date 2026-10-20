package replique.lang;

/** A function called with a number of arguments it has no arity for. */
public final class ArityException extends IllegalArgumentException {
    public final int actual;
    public final String name;

    public ArityException(int actual, String name) {
        super("Wrong number of args (" + actual + ") passed to: " + name);
        this.actual = actual;
        this.name = name;
    }
}
