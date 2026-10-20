// The overloads differ only in their arity: written by a script, kept as source.
package replique.lang;

import java.util.Arrays;

/**
 * The base of a variadic function: {@code (fn [a b & more] ...)}.
 *
 * <p>A call with at least {@link #requiredArity} arguments goes to
 * {@link #doInvoke} with every argument in one array; the function's body
 * takes its fixed parameters from the front of the array and its rest
 * parameter from {@link #rest}. A call with fewer goes to the fixed arities,
 * which a compiled function overrides when it has any, and otherwise throws
 * {@link ArityException}.
 */
public abstract class RestFn extends AFn {

    /** The number of fixed parameters of the variadic arity. */
    public abstract int requiredArity();

    /** The variadic arity's body, given every argument. */
    public abstract Object doInvoke(Object[] args);

    /**
     * The rest parameter of a call: the arguments after the first
     * {@code required}, or null when there are none, as in Clojure.
     */
    public static Object[] rest(Object[] args, int required) {
        return args.length <= required ? null : Arrays.copyOfRange(args, required, args.length);
    }

    @Override
    public Object invoke() {
        if (requiredArity() <= 0) return doInvoke(new Object[0]);
        return super.invoke();
    }

    @Override
    public Object invoke(Object a1) {
        if (requiredArity() <= 1) return doInvoke(new Object[]{a1});
        return super.invoke(a1);
    }

    @Override
    public Object invoke(Object a1, Object a2) {
        if (requiredArity() <= 2) return doInvoke(new Object[]{a1, a2});
        return super.invoke(a1, a2);
    }

    @Override
    public Object invoke(Object a1, Object a2, Object a3) {
        if (requiredArity() <= 3) return doInvoke(new Object[]{a1, a2, a3});
        return super.invoke(a1, a2, a3);
    }

    @Override
    public Object invoke(Object a1, Object a2, Object a3, Object a4) {
        if (requiredArity() <= 4) return doInvoke(new Object[]{a1, a2, a3, a4});
        return super.invoke(a1, a2, a3, a4);
    }

    @Override
    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5) {
        if (requiredArity() <= 5) return doInvoke(new Object[]{a1, a2, a3, a4, a5});
        return super.invoke(a1, a2, a3, a4, a5);
    }

    @Override
    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6) {
        if (requiredArity() <= 6) return doInvoke(new Object[]{a1, a2, a3, a4, a5, a6});
        return super.invoke(a1, a2, a3, a4, a5, a6);
    }

    @Override
    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7) {
        if (requiredArity() <= 7) return doInvoke(new Object[]{a1, a2, a3, a4, a5, a6, a7});
        return super.invoke(a1, a2, a3, a4, a5, a6, a7);
    }

    @Override
    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8) {
        if (requiredArity() <= 8) return doInvoke(new Object[]{a1, a2, a3, a4, a5, a6, a7, a8});
        return super.invoke(a1, a2, a3, a4, a5, a6, a7, a8);
    }

    @Override
    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9) {
        if (requiredArity() <= 9) return doInvoke(new Object[]{a1, a2, a3, a4, a5, a6, a7, a8, a9});
        return super.invoke(a1, a2, a3, a4, a5, a6, a7, a8, a9);
    }

    @Override
    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10) {
        if (requiredArity() <= 10) return doInvoke(new Object[]{a1, a2, a3, a4, a5, a6, a7, a8, a9, a10});
        return super.invoke(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
    }

    @Override
    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11) {
        if (requiredArity() <= 11) return doInvoke(new Object[]{a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11});
        return super.invoke(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11);
    }

    @Override
    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12) {
        if (requiredArity() <= 12) return doInvoke(new Object[]{a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12});
        return super.invoke(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12);
    }

    @Override
    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13) {
        if (requiredArity() <= 13) return doInvoke(new Object[]{a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13});
        return super.invoke(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13);
    }

    @Override
    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14) {
        if (requiredArity() <= 14) return doInvoke(new Object[]{a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14});
        return super.invoke(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14);
    }

    @Override
    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15) {
        if (requiredArity() <= 15) return doInvoke(new Object[]{a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15});
        return super.invoke(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15);
    }

    @Override
    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16) {
        if (requiredArity() <= 16) return doInvoke(new Object[]{a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16});
        return super.invoke(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16);
    }

    @Override
    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17) {
        if (requiredArity() <= 17) return doInvoke(new Object[]{a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17});
        return super.invoke(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17);
    }

    @Override
    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18) {
        if (requiredArity() <= 18) return doInvoke(new Object[]{a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18});
        return super.invoke(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18);
    }

    @Override
    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19) {
        if (requiredArity() <= 19) return doInvoke(new Object[]{a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19});
        return super.invoke(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19);
    }

    @Override
    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20) {
        if (requiredArity() <= 20) return doInvoke(new Object[]{a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20});
        return super.invoke(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20);
    }

    @Override
    public Object invoke(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object... rest) {
        Object[] all = Arrays.copyOf(new Object[]{a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20}, 20 + rest.length);
        System.arraycopy(rest, 0, all, 20, rest.length);
        if (requiredArity() <= all.length) return doInvoke(all);
        return super.invoke(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, rest);
    }

    @Override
    public Object applyTo(Object[] a) {
        if (requiredArity() <= a.length) return doInvoke(a);
        return super.applyTo(a);
    }
}
