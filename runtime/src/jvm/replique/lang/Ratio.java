package replique.lang;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

/**
 * A ratio, 1/2: the JVM's result of dividing integers that don't divide
 * (doc/language.md §5). A value only, so far: arithmetic comes with the
 * kernel's numbers. Normalised by whoever makes one: lowest terms, a
 * positive denominator other than 1.
 */
public final class Ratio extends Number {
    public final BigInteger numerator;
    public final BigInteger denominator;

    public Ratio(BigInteger numerator, BigInteger denominator) {
        this.numerator = numerator;
        this.denominator = denominator;
    }

    @Override
    public double doubleValue() {
        return new BigDecimal(numerator).divide(new BigDecimal(denominator), MathContext.DECIMAL64).doubleValue();
    }

    @Override
    public float floatValue() {
        return (float) doubleValue();
    }

    @Override
    public long longValue() {
        return numerator.divide(denominator).longValue();
    }

    @Override
    public int intValue() {
        return (int) longValue();
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof Ratio r && numerator.equals(r.numerator) && denominator.equals(r.denominator);
    }

    @Override
    public int hashCode() {
        return numerator.hashCode() ^ denominator.hashCode();
    }

    @Override
    public String toString() {
        return numerator + "/" + denominator;
    }
}
