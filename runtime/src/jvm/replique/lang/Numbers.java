package replique.lang;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * The kernel's arithmetic: host numbers with Clojure's semantics on the JVM
 * (doc/language.md §5). Longs throw on overflow, integers that don't divide
 * make a {@link Ratio}, and mixing two kinds of number gives the more
 * contagious one: long, then BigInteger, ratio, BigDecimal and double.
 * Integer, Short and Byte count as longs, Float as a double, which is what
 * interop returns.
 *
 * <p>{@link #equal} is {@code =}: numbers of different categories (integer,
 * ratio, decimal, floating) are never equal, so {@code (= 1 1.0)} is false.
 */
public final class Numbers {
    private Numbers() {}

    private static final int LONG = 0, BIGINT = 1, RATIO = 2, DECIMAL = 3, DOUBLE = 4;

    private static int rank(Number x) {
        if (x instanceof Long || x instanceof Integer || x instanceof Short || x instanceof Byte) return LONG;
        if (x instanceof Double || x instanceof Float) return DOUBLE;
        if (x instanceof BigInteger) return BIGINT;
        if (x instanceof Ratio) return RATIO;
        if (x instanceof BigDecimal) return DECIMAL;
        throw new IllegalArgumentException("not a number the kernel knows: " + x.getClass().getName());
    }

    private static int rank(Number a, Number b) {
        return Math.max(rank(a), rank(b));
    }

    private static ArithmeticException overflow() {
        return new ArithmeticException("integer overflow");
    }

    private static BigInteger big(Number x) {
        return x instanceof BigInteger b ? b : BigInteger.valueOf(x.longValue());
    }

    private static BigDecimal decimal(Number x) {
        if (x instanceof BigDecimal d) return d;
        if (x instanceof BigInteger b) return new BigDecimal(b);
        if (x instanceof Ratio r) return new BigDecimal(r.numerator).divide(new BigDecimal(r.denominator));
        return BigDecimal.valueOf(x.longValue());
    }

    private static BigInteger num(Number x) {
        return x instanceof Ratio r ? r.numerator : big(x);
    }

    private static BigInteger den(Number x) {
        return x instanceof Ratio r ? r.denominator : BigInteger.ONE;
    }

    /** n/d in lowest terms: a BigInteger when d divides n, else a Ratio. */
    static Number ratio(BigInteger n, BigInteger d) {
        if (d.signum() == 0) throw new ArithmeticException("Divide by zero");
        BigInteger g = n.gcd(d);
        n = n.divide(g);
        d = d.divide(g);
        if (d.signum() < 0) {
            n = n.negate();
            d = d.negate();
        }
        return d.equals(BigInteger.ONE) ? n : new Ratio(n, d);
    }

    public static Number add(Object x, Object y) {
        Number a = (Number) x, b = (Number) y;
        switch (rank(a, b)) {
            case LONG: {
                long l = a.longValue(), r = b.longValue(), s = l + r;
                if (((l ^ s) & (r ^ s)) < 0) throw overflow();
                return s;
            }
            case BIGINT: return big(a).add(big(b));
            case RATIO: return ratio(num(a).multiply(den(b)).add(num(b).multiply(den(a))), den(a).multiply(den(b)));
            case DECIMAL: return decimal(a).add(decimal(b));
            default: return a.doubleValue() + b.doubleValue();
        }
    }

    public static Number subtract(Object x, Object y) {
        Number a = (Number) x, b = (Number) y;
        switch (rank(a, b)) {
            case LONG: {
                long l = a.longValue(), r = b.longValue(), s = l - r;
                if (((l ^ r) & (l ^ s)) < 0) throw overflow();
                return s;
            }
            case BIGINT: return big(a).subtract(big(b));
            case RATIO: return ratio(num(a).multiply(den(b)).subtract(num(b).multiply(den(a))), den(a).multiply(den(b)));
            case DECIMAL: return decimal(a).subtract(decimal(b));
            default: return a.doubleValue() - b.doubleValue();
        }
    }

    public static Number multiply(Object x, Object y) {
        Number a = (Number) x, b = (Number) y;
        switch (rank(a, b)) {
            case LONG:
                try {
                    return Math.multiplyExact(a.longValue(), b.longValue());
                } catch (ArithmeticException e) {
                    throw overflow();
                }
            case BIGINT: return big(a).multiply(big(b));
            case RATIO: return ratio(num(a).multiply(num(b)), den(a).multiply(den(b)));
            case DECIMAL: return decimal(a).multiply(decimal(b));
            default: return a.doubleValue() * b.doubleValue();
        }
    }

    public static Number divide(Object x, Object y) {
        Number a = (Number) x, b = (Number) y;
        switch (rank(a, b)) {
            case LONG: {
                long n = a.longValue(), d = b.longValue();
                if (d == 0) throw new ArithmeticException("Divide by zero");
                if (n % d == 0 && !(n == Long.MIN_VALUE && d == -1)) return n / d;
                return ratio(BigInteger.valueOf(n), BigInteger.valueOf(d));
            }
            case BIGINT:
            case RATIO: return ratio(num(a).multiply(den(b)), den(a).multiply(num(b)));
            case DECIMAL: return decimal(a).divide(decimal(b));
            default: return a.doubleValue() / b.doubleValue();
        }
    }

    public static Number negate(Object x) {
        return subtract(0L, x);
    }

    public static Number inc(Object x) {
        if (x instanceof Long l) {
            if (l == Long.MAX_VALUE) throw overflow();
            return l + 1;
        }
        return add(x, 1L);
    }

    public static Number dec(Object x) {
        if (x instanceof Long l) {
            if (l == Long.MIN_VALUE) throw overflow();
            return l - 1;
        }
        return subtract(x, 1L);
    }

    /** The sign of a - b, for two numbers that aren't doubles. */
    private static int compare(int rank, Number a, Number b) {
        switch (rank) {
            case LONG: return Long.compare(a.longValue(), b.longValue());
            case BIGINT: return big(a).compareTo(big(b));
            case RATIO: return num(a).multiply(den(b)).compareTo(num(b).multiply(den(a)));
            default: return decimal(a).compareTo(decimal(b));
        }
    }

    public static boolean lt(Object x, Object y) {
        Number a = (Number) x, b = (Number) y;
        int k = rank(a, b);
        return k == DOUBLE ? a.doubleValue() < b.doubleValue() : compare(k, a, b) < 0;
    }

    public static boolean lte(Object x, Object y) {
        Number a = (Number) x, b = (Number) y;
        int k = rank(a, b);
        return k == DOUBLE ? a.doubleValue() <= b.doubleValue() : compare(k, a, b) <= 0;
    }

    public static boolean gt(Object x, Object y) {
        return lt(y, x);
    }

    public static boolean gte(Object x, Object y) {
        return lte(y, x);
    }

    /** {@code ==}: the same number, whatever its kind. */
    public static boolean equiv(Object x, Object y) {
        Number a = (Number) x, b = (Number) y;
        int k = rank(a, b);
        return k == DOUBLE ? a.doubleValue() == b.doubleValue() : compare(k, a, b) == 0;
    }

    private static int category(Number x) {
        int r = rank(x);
        return r == BIGINT ? LONG : r;
    }

    /** {@code =} on two numbers: the same category, and the same number. */
    public static boolean equal(Number a, Number b) {
        return category(a) == category(b) && equiv(a, b);
    }
}
