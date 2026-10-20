// The overloads differ only in their arity: written by a script, kept as source.
package replique.lang;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodType;
import java.util.Arrays;

/**
 * The base of every compiled function. Each arity a function doesn't define
 * throws {@link ArityException}; a compiled function overrides the arities it
 * has. {@link #applyTo} dispatches on the number of arguments.
 */
public abstract class AFn implements IFn {

    /** The name an arity error reports: ns/name for a named function. */
    public String fnName() {
        return getClass().getName();
    }

    /**
     * A handle that calls this function's code for {@code type} directly,
     * without going through {@code invoke}, or null. The dev-mode linker
     * ({@link Linker}) links a call site to it, adapted to the site's type
     * with {@code asType}. A compiled function that closes over nothing
     * returns its static method for the arity; the default is null, and the
     * linker then calls {@code invoke}.
     */
    public MethodHandle linkTo(MethodType type) {
        return null;
    }

    protected final ArityException arity(int n) {
        return new ArityException(n, fnName());
    }

    public Object invoke() {
        throw arity(0);
    }

    public Object invoke(Object a1) {
        throw arity(1);
    }

    public Object invoke(Object a1, Object a2) {
        throw arity(2);
    }

    public Object invoke(Object a1, Object a2, Object a3) {
        throw arity(3);
    }

    public Object invoke(Object a1, Object a2, Object a3, Object a4) {
        throw arity(4);
    }

    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5) {
        throw arity(5);
    }

    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6) {
        throw arity(6);
    }

    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7) {
        throw arity(7);
    }

    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8) {
        throw arity(8);
    }

    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9) {
        throw arity(9);
    }

    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10) {
        throw arity(10);
    }

    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11) {
        throw arity(11);
    }

    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12) {
        throw arity(12);
    }

    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13) {
        throw arity(13);
    }

    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14) {
        throw arity(14);
    }

    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15) {
        throw arity(15);
    }

    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16) {
        throw arity(16);
    }

    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17) {
        throw arity(17);
    }

    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18) {
        throw arity(18);
    }

    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19) {
        throw arity(19);
    }

    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20) {
        throw arity(20);
    }

    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object... rest) {
        throw arity(20 + rest.length);
    }

    public Object applyTo(Object[] a) {
        switch (a.length) {
            case 0: return invoke();
            case 1: return invoke(a[0]);
            case 2: return invoke(a[0], a[1]);
            case 3: return invoke(a[0], a[1], a[2]);
            case 4: return invoke(a[0], a[1], a[2], a[3]);
            case 5: return invoke(a[0], a[1], a[2], a[3], a[4]);
            case 6: return invoke(a[0], a[1], a[2], a[3], a[4], a[5]);
            case 7: return invoke(a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
            case 8: return invoke(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
            case 9: return invoke(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
            case 10: return invoke(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9]);
            case 11: return invoke(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10]);
            case 12: return invoke(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11]);
            case 13: return invoke(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12]);
            case 14: return invoke(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13]);
            case 15: return invoke(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14]);
            case 16: return invoke(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15]);
            case 17: return invoke(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16]);
            case 18: return invoke(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17]);
            case 19: return invoke(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18]);
            case 20: return invoke(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19]);
            default: return invoke(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19],
                                   Arrays.copyOfRange(a, 20, a.length));
        }
    }
}
