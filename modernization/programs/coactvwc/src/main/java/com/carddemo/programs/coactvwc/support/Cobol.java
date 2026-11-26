package com.carddemo.programs.coactvwc.support;

import java.math.BigDecimal;
import java.math.RoundingMode;

/** Helpers that reproduce COBOL MOVE / STRING / class-test semantics on fixed-width text fields. */
public final class Cobol {

    public static final char LOW_VALUE = '\u0000';

    private Cobol() {
    }

    public static String spaces(int len) {
        return " ".repeat(len);
    }

    public static String lowValues(int len) {
        return String.valueOf(LOW_VALUE).repeat(len);
    }

    /** Alphanumeric MOVE: left-justify, pad with spaces or truncate on the right. {@code null} = LOW-VALUES. */
    public static String alnum(String src, int len) {
        if (src == null) {
            return lowValues(len);
        }
        if (src.length() >= len) {
            return src.substring(0, len);
        }
        return src + spaces(len - src.length());
    }

    public static boolean isAll(String s, char c) {
        if (s == null || s.isEmpty()) {
            return false;
        }
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) != c) {
                return false;
            }
        }
        return true;
    }

    public static boolean isSpaces(String s) {
        return isAll(s, ' ');
    }

    public static boolean isLowValues(String s) {
        return isAll(s, LOW_VALUE);
    }

    /** {@code IS NUMERIC} class test on an alphanumeric (USAGE DISPLAY, unsigned) item. */
    public static boolean isNumeric(String s) {
        if (s == null || s.isEmpty()) {
            return false;
        }
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c < '0' || c > '9') {
                return false;
            }
        }
        return true;
    }

    /**
     * MOVE of an unsigned/signed integer to a PIC 9(n) DISPLAY or alphanumeric item: absolute value,
     * zero-filled, high-order digits truncated.
     */
    public static String digits(long value, int len) {
        String s = Long.toString(Math.abs(value));
        if (s.length() > len) {
            return s.substring(s.length() - len);
        }
        return "0".repeat(len - s.length()) + s;
    }

    /** Lenient read of a PIC 9(n) DISPLAY field: non-digit bytes count as zero (like a numeric MOVE of junk). */
    public static long toLong(String s) {
        long v = 0;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            v = v * 10 + ((c >= '0' && c <= '9') ? c - '0' : 0);
        }
        return v;
    }

    /**
     * {@code STRING ... DELIMITED BY SIZE INTO target}: the concatenation overwrites the target from the left,
     * stops at the end of the target (overflow), and leaves positions beyond the data unchanged.
     */
    public static String stringInto(String target, String... parts) {
        StringBuilder sb = new StringBuilder();
        for (String p : parts) {
            sb.append(p);
        }
        String data = sb.length() > target.length() ? sb.substring(0, target.length()) : sb.toString();
        return data + target.substring(data.length());
    }

    /** Decode a signed zoned-decimal (trailing overpunch) DISPLAY field, e.g. {@code PIC S9(10)V99}. */
    public static BigDecimal zonedDecimal(String field, int scale) {
        if (field == null || field.isEmpty()) {
            return BigDecimal.ZERO.setScale(scale);
        }
        String body = field.substring(0, field.length() - 1);
        char last = field.charAt(field.length() - 1);
        int lastDigit;
        boolean negative = false;
        if (last >= '0' && last <= '9') {
            lastDigit = last - '0';
        } else if (last == '{') {
            lastDigit = 0;
        } else if (last >= 'A' && last <= 'I') {
            lastDigit = last - 'A' + 1;
        } else if (last == '}') {
            lastDigit = 0;
            negative = true;
        } else if (last >= 'J' && last <= 'R') {
            lastDigit = last - 'J' + 1;
            negative = true;
        } else if (last >= 'p' && last <= 'y') {
            lastDigit = last - 'p';
            negative = true;
        } else {
            lastDigit = 0;
        }
        BigDecimal unscaled = new BigDecimal(toLong(body) * 10 + lastDigit);
        BigDecimal v = unscaled.movePointLeft(scale);
        return negative ? v.negate() : v;
    }

    /** Encode a value as signed zoned decimal with trailing overpunch ({@code {A-I} / }J-R}). */
    public static String toZonedDecimal(BigDecimal value, int integerDigits, int scale) {
        BigDecimal abs = value.abs().setScale(scale, RoundingMode.DOWN);
        String d = digits(abs.unscaledValue().longValueExact(), integerDigits + scale);
        int last = d.charAt(d.length() - 1) - '0';
        char over;
        if (value.signum() < 0) {
            over = last == 0 ? '}' : (char) ('J' + last - 1);
        } else {
            over = last == 0 ? '{' : (char) ('A' + last - 1);
        }
        return d.substring(0, d.length() - 1) + over;
    }

    /**
     * MOVE of a numeric value to {@code PIC +ZZZ,ZZZ,ZZZ.99}: fixed leading sign, nine integer positions with
     * zero suppression (commas suppressed while still suppressing), two fraction digits, high-order integer
     * digits beyond nine truncated, extra fraction digits truncated.
     */
    public static String editPlusZzzComma99(BigDecimal value) {
        BigDecimal abs = value.abs().setScale(2, RoundingMode.DOWN);
        long unscaled = abs.unscaledValue().mod(java.math.BigInteger.valueOf(100_000_000_000L)).longValue();
        String d = digits(unscaled, 11);
        StringBuilder out = new StringBuilder(15);
        out.append(value.signum() < 0 ? '-' : '+');
        boolean significant = false;
        for (int i = 0; i < 9; i++) {
            if (i == 3 || i == 6) {
                out.append(significant ? ',' : ' ');
            }
            char c = d.charAt(i);
            if (c != '0') {
                significant = true;
            }
            out.append(significant ? c : ' ');
        }
        out.append('.').append(d, 9, 11);
        return out.toString();
    }
}
