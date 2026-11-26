package com.carddemo.programs.cotrtupc.support;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Arrays;
import java.util.Locale;

/** COBOL alphanumeric / numeric helpers. LOW-VALUES are modelled as {@code '\u0000'}. */
public final class CobolText {

    public static final char LOW_VALUE = '\u0000';

    private CobolText() {
    }

    public static String spaces(int n) {
        return repeat(' ', n);
    }

    public static String lowValues(int n) {
        return repeat(LOW_VALUE, n);
    }

    public static String repeat(char c, int n) {
        char[] a = new char[n];
        Arrays.fill(a, c);
        return new String(a);
    }

    /** MOVE alphanumeric to PIC X(n): left-justify, pad with spaces, truncate on the right. */
    public static String pic(String value, int n) {
        String v = value == null ? "" : value;
        if (v.length() >= n) {
            return v.substring(0, n);
        }
        return v + spaces(n - v.length());
    }

    public static boolean isAll(String s, char c) {
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) != c) {
                return false;
            }
        }
        return true;
    }

    /** {@code x = SPACES}. */
    public static boolean isSpaces(String s) {
        return isAll(s, ' ');
    }

    /** {@code x = LOW-VALUES}. */
    public static boolean isLowValues(String s) {
        return isAll(s, LOW_VALUE);
    }

    /** FUNCTION TRIM: removes leading and trailing spaces only (all-space argument gives length 0). */
    public static String trim(String s) {
        int b = 0;
        int e = s.length();
        while (b < e && s.charAt(b) == ' ') {
            b++;
        }
        while (e > b && s.charAt(e - 1) == ' ') {
            e--;
        }
        return s.substring(b, e);
    }

    /** Alphanumeric comparison: the shorter operand is padded with spaces. */
    public static boolean eq(String a, String b) {
        int n = Math.max(a.length(), b.length());
        return pic(a, n).equals(pic(b, n));
    }

    /** FUNCTION UPPER-CASE (single-byte a-z only). */
    public static String upper(String s) {
        StringBuilder sb = new StringBuilder(s.length());
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            sb.append(c >= 'a' && c <= 'z' ? (char) (c - 32) : c);
        }
        return sb.toString();
    }

    /**
     * FUNCTION TEST-NUMVAL: 0 when the argument is a valid NUMVAL argument, otherwise the (1-based)
     * position of the first offending character (or length + 1 when the string ends prematurely).
     */
    public static int testNumval(String s) {
        int n = s.length();
        int i = skipSpaces(s, 0);
        boolean leadingSign = false;
        if (i < n && (s.charAt(i) == '+' || s.charAt(i) == '-')) {
            leadingSign = true;
            i = skipSpaces(s, i + 1);
        }
        int digits = 0;
        boolean dot = false;
        while (i < n) {
            char c = s.charAt(i);
            if (c >= '0' && c <= '9') {
                digits++;
                i++;
            } else if (c == '.' && !dot) {
                dot = true;
                i++;
            } else {
                break;
            }
        }
        if (digits == 0) {
            return i + 1;
        }
        i = skipSpaces(s, i);
        if (i < n && !leadingSign) {
            char c = s.charAt(i);
            if (c == '+' || c == '-') {
                i = skipSpaces(s, i + 1);
            } else if (i + 1 < n) {
                String two = s.substring(i, i + 2).toUpperCase(Locale.ROOT);
                if (two.equals("CR") || two.equals("DB")) {
                    i = skipSpaces(s, i + 2);
                }
            }
        }
        return i < n ? i + 1 : 0;
    }

    /** FUNCTION NUMVAL. Caller must have validated the argument with {@link #testNumval}. */
    public static BigDecimal numval(String s) {
        if (testNumval(s) != 0) {
            throw new IllegalArgumentException("NUMVAL argument not valid: '" + s + "'");
        }
        String t = s.replace(" ", "").toUpperCase(Locale.ROOT);
        boolean negative = false;
        if (t.startsWith("+") || t.startsWith("-")) {
            negative = t.charAt(0) == '-';
            t = t.substring(1);
        } else if (t.endsWith("CR") || t.endsWith("DB")) {
            negative = true;
            t = t.substring(0, t.length() - 2);
        } else if (t.endsWith("+") || t.endsWith("-")) {
            negative = t.endsWith("-");
            t = t.substring(0, t.length() - 1);
        }
        if (t.startsWith(".")) {
            t = "0" + t;
        }
        if (t.endsWith(".")) {
            t = t.substring(0, t.length() - 1);
        }
        BigDecimal v = new BigDecimal(t);
        return negative ? v.negate() : v;
    }

    /** COMPUTE/MOVE into an unsigned PIC 9(digits): sign dropped, decimals and high-order digits truncated. */
    public static String toUnsignedDisplay(BigDecimal value, int digits) {
        BigDecimal whole = value.abs().setScale(0, RoundingMode.DOWN);
        String s = whole.toPlainString();
        if (s.length() > digits) {
            s = s.substring(s.length() - digits);
        }
        return repeat('0', digits - s.length()) + s;
    }

    /** MOVE of a binary/display integer into PIC ----9 (floating minus, four digit positions). */
    public static String editMinus4Z9(long value) {
        long mag = Math.abs(value) % 10000;
        String s = Long.toString(mag);
        if (value < 0 && mag != 0) {
            s = "-" + s;
        }
        return pic("", 5 - s.length()) + s;
    }

    /** STRING ... DELIMITED BY SIZE INTO target: overwrites from position 1, keeps the rest. */
    public static String stringInto(String target, String... parts) {
        StringBuilder sb = new StringBuilder(target);
        int pos = 0;
        outer:
        for (String p : parts) {
            for (int i = 0; i < p.length(); i++) {
                if (pos >= sb.length()) {
                    break outer;
                }
                sb.setCharAt(pos++, p.charAt(i));
            }
        }
        return sb.toString();
    }

    private static int skipSpaces(String s, int i) {
        while (i < s.length() && s.charAt(i) == ' ') {
            i++;
        }
        return i;
    }
}
