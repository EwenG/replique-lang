package replique.lang;

/**
 * What {@code ex-info} throws: an exception carrying data (doc/runtime.md §2).
 * The data is any value until core's maps exist (M2), and may be nil.
 */
public class ExceptionInfo extends RuntimeException {
    public final Object data;

    public ExceptionInfo(String message, Object data) {
        this(message, data, null);
    }

    public ExceptionInfo(String message, Object data, Throwable cause) {
        super(message, cause);
        this.data = data;
    }
}
