package replique.lang;

import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * The kernel's printer: what the conformance harness reads results with
 * (doc/conformance.md §3), before core and its printer exist.
 *
 * <p>It prints what the kernel knows: nil, booleans, numbers, characters,
 * strings, keywords, symbols, vars, functions and host arrays. Anything else
 * prints as its kind, #object[class-name]. The text reads back, with the
 * Replique reader and the tags object and array, to the same value; numbers
 * keep their host type (1, 1.0, 1/2, 1N, 1M). runtime.js prints the same way,
 * and both escape strings alike.
 */
public final class Printer {
    private Printer() {}

    public static String print(Object x) {
        StringBuilder sb = new StringBuilder();
        print(x, sb);
        return sb.toString();
    }

    public static void print(Object x, StringBuilder sb) {
        if (x == null) {
            sb.append("nil");
        } else if (x instanceof Boolean || x instanceof Long || x instanceof Integer
                || x instanceof Short || x instanceof Byte) {
            sb.append(x);
        } else if (x instanceof Double d) {
            printDouble(d, sb);
        } else if (x instanceof Float f) {
            printDouble(f.doubleValue(), sb);
        } else if (x instanceof BigInteger) {
            sb.append(x).append('N');
        } else if (x instanceof BigDecimal) {
            sb.append(x).append('M');
        } else if (x instanceof Ratio) {
            sb.append(x);
        } else if (x instanceof String s) {
            printString(s, sb);
        } else if (x instanceof Character c) {
            printChar(c, sb);
        } else if (x instanceof Keyword || x instanceof Symbol) {
            sb.append(x);
        } else if (x instanceof Var v) {
            sb.append("(var ").append(v.ns.name).append('/').append(v.name).append(')');
        } else if (x instanceof IFn) {
            sb.append("#object[fn]");
        } else if (x.getClass().isArray()) {
            sb.append("#array[");
            for (int i = 0, n = Array.getLength(x); i < n; i++) {
                if (i > 0) sb.append(' ');
                print(Array.get(x, i), sb);
            }
            sb.append(']');
        } else {
            String n = x.getClass().getName();
            int hidden = n.indexOf('/');
            sb.append("#object[").append(hidden < 0 ? n : n.substring(0, hidden)).append(']');
        }
    }

    private static void printDouble(double d, StringBuilder sb) {
        if (Double.isNaN(d)) sb.append("##NaN");
        else if (d == Double.POSITIVE_INFINITY) sb.append("##Inf");
        else if (d == Double.NEGATIVE_INFINITY) sb.append("##-Inf");
        else sb.append(d);
    }

    private static void printString(String s, StringBuilder sb) {
        sb.append('"');
        for (int i = 0, n = s.length(); i < n; i++) {
            char c = s.charAt(i);
            switch (c) {
                case '"' -> sb.append("\\\"");
                case '\\' -> sb.append("\\\\");
                case '\n' -> sb.append("\\n");
                case '\t' -> sb.append("\\t");
                case '\r' -> sb.append("\\r");
                case '\b' -> sb.append("\\b");
                case '\f' -> sb.append("\\f");
                default -> {
                    if (Character.isHighSurrogate(c) && i + 1 < n && Character.isLowSurrogate(s.charAt(i + 1))) {
                        sb.append(c).append(s.charAt(++i));
                    } else if (needsEscape(c)) {
                        unicodeEscape(c, sb);
                    } else {
                        sb.append(c);
                    }
                }
            }
        }
        sb.append('"');
    }

    private static void printChar(char c, StringBuilder sb) {
        switch (c) {
            case '\n' -> sb.append("\\newline");
            case ' ' -> sb.append("\\space");
            case '\t' -> sb.append("\\tab");
            case '\b' -> sb.append("\\backspace");
            case '\f' -> sb.append("\\formfeed");
            case '\r' -> sb.append("\\return");
            default -> {
                if (needsEscape(c)) unicodeEscape(c, sb);
                else sb.append('\\').append(c);
            }
        }
    }

    /** Control characters and lone surrogates, which print as \\uXXXX. */
    private static boolean needsEscape(char c) {
        return c < 0x20 || c == 0x7f || Character.isSurrogate(c);
    }

    private static void unicodeEscape(char c, StringBuilder sb) {
        String hex = Integer.toHexString(c);
        sb.append("\\u");
        for (int i = hex.length(); i < 4; i++) sb.append('0');
        sb.append(hex);
    }
}
