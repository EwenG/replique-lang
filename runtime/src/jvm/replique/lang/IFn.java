// The overloads differ only in their arity: written by a script, kept as source.
package replique.lang;

/**
 * A function value: the calling convention of doc/runtime.md §2.
 *
 * <p>A call with n arguments, n at most 20, is {@code invoke} with n
 * arguments. A call with more passes the first 20 and the others in an
 * array. {@link #applyTo} calls with the arguments in an array, whatever
 * their number.
 *
 * <p>Calls the compiler can see the target of don't come here: in dev mode
 * they are invokedynamic call sites linked to the function's own entry point
 * ({@link AFn#linkTo}). This interface is the path for a function used as a
 * value.
 */
public interface IFn {
    Object invoke();
    Object invoke(Object a1);
    Object invoke(Object a1, Object a2);
    Object invoke(Object a1, Object a2, Object a3);
    Object invoke(Object a1, Object a2, Object a3, Object a4);
    Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5);
    Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6);
    Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7);
    Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8);
    Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9);
    Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10);
    Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11);
    Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12);
    Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13);
    Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14);
    Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15);
    Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16);
    Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17);
    Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18);
    Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19);
    Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20);

    Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object... rest);

    Object applyTo(Object[] args);
}
