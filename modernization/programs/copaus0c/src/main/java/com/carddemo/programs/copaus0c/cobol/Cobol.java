package com.carddemo.programs.copaus0c.cobol;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/** COBOL data-movement primitives used by the port (MOVE, STRING, IS NUMERIC, edited PICs). */
public final class Cobol {

    public static final char LOW_VALUE = '\0';

    private Cobol() {
    }

    public static String lowValues(int length) {
        return String.valueOf(LOW_VALUE).repeat(length);
    }

    public static String spaces(int length) {
        return " ".repeat(length);
    }

    /** Alphanumeric MOVE: left-justify, pad with spaces, truncate on the right. */
    public static String moveX(String source, int length) {
        String s = source == null ? "" : source;
        if (s.length() >= length) {
            return s.substring(0, length);
        }
        return s + spaces(length - s.length());
    }

    /** True when every character is a space or every character is LOW-VALUES (COBOL "= SPACES OR LOW-VALUES"). */
    public static boolean isSpacesOrLowValues(String value) {
        return isAll(value, ' ') || isAll(value, LOW_VALUE);
    }

    public static boolean isAll(String value, char c) {
        if (value == null) {
            return c == LOW_VALUE;
        }
        for (int i = 0; i < value.length(); i++) {
            if (value.charAt(i) != c) {
                return false;
            }
        }
        return true;
    }

    /** Class test {@code IS NUMERIC} for an unsigned display field: every character must be 0-9. */
    public static boolean isNumeric(String value) {
        if (value == null || value.isEmpty()) {
            return false;
        }
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            if (c < '0' || c > '9') {
                return false;
            }
        }
        return true;
    }

    /** Numeric value of a display field; non-digit characters (e.g. LOW-VALUES) count as zero. */
    public static long displayValue(String value) {
        long v = 0;
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            v = v * 10 + ((c >= '0' && c <= '9') ? c - '0' : 0);
        }
        return v;
    }

    /** MOVE of an integer to an unsigned PIC 9(n) display item: sign dropped, high-order digits truncated. */
    public static String moveToUnsignedDisplay(long value, int digits) {
        BigInteger mod = BigInteger.TEN.pow(digits);
        BigInteger v = BigInteger.valueOf(value).abs().mod(mod);
        String s = v.toString();
        return "0".repeat(digits - s.length()) + s;
    }

    /**
     * MOVE to a numeric-edited item of the form {@code -z...z9.99}: fixed leading minus sign (space when
     * positive), {@code intDigits} integer positions with leading zero suppression except the last, and two
     * decimals. High-order integer digits are truncated and extra decimals truncated, as COBOL MOVE does.
     */
    public static String editMinusZ9V99(BigDecimal value, int intDigits) {
        BigDecimal v = value == null ? BigDecimal.ZERO : value;
        boolean negative = v.signum() < 0;
        BigInteger cents = v.abs().setScale(2, RoundingMode.DOWN).unscaledValue()
                .mod(BigInteger.TEN.pow(intDigits + 2));
        String digits = cents.toString();
        digits = "0".repeat(intDigits + 2 - digits.length()) + digits;
        StringBuilder intPart = new StringBuilder(digits.substring(0, intDigits));
        for (int i = 0; i < intDigits - 1 && intPart.charAt(i) == '0'; i++) {
            intPart.setCharAt(i, ' ');
        }
        return (negative ? "-" : " ") + intPart + "." + digits.substring(intDigits);
    }

    /** Characters of {@code source} before the first occurrence of {@code delimiter} (STRING ... DELIMITED BY). */
    public static String delimitedBy(String source, String delimiter) {
        int idx = source.indexOf(delimiter);
        return idx < 0 ? source : source.substring(0, idx);
    }

    /**
     * {@code STRING parts... DELIMITED BY SIZE INTO target}: characters are written from position 1 until
     * the sources are exhausted or the target is full; target positions that are not written keep their
     * previous content (STRING never pads).
     */
    public static String stringInto(String target, String... parts) {
        StringBuilder out = new StringBuilder(target);
        int pos = 0;
        for (String part : parts) {
            for (int i = 0; i < part.length() && pos < out.length(); i++) {
                out.setCharAt(pos++, part.charAt(i));
            }
        }
        return out.toString();
    }
}
