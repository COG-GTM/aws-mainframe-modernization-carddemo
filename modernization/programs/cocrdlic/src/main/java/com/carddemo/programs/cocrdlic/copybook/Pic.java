package com.carddemo.programs.cocrdlic.copybook;

/** Helpers reproducing COBOL MOVE / class-test semantics on fixed-width PIC X and PIC 9 fields. */
public final class Pic {
    public static final char LOW_VALUE = '\u0000';

    private Pic() {
    }

    public static String spaces(int n) {
        return " ".repeat(n);
    }

    public static String lowValues(int n) {
        return String.valueOf(LOW_VALUE).repeat(n);
    }

    /** MOVE alphanumeric to PIC X(n): left-justify, truncate on the right, pad with spaces. */
    public static String x(String src, int n) {
        if (src == null) {
            return spaces(n);
        }
        if (src.length() >= n) {
            return src.substring(0, n);
        }
        return src + spaces(n - src.length());
    }

    /** MOVE unsigned integer to PIC 9(n) DISPLAY: high-order truncation, zero padded. */
    public static String nine(long value, int digits) {
        long mod = 1;
        for (int i = 0; i < digits; i++) {
            mod *= 10;
        }
        long v = Math.abs(value) % mod;
        String s = Long.toString(v);
        return "0".repeat(digits - s.length()) + s;
    }

    /** Value held by an unsigned PIC 9(n) field after storing {@code value} (sign dropped, high-order truncated). */
    public static long truncUnsigned(long value, int digits) {
        return Long.parseLong(nine(value, digits));
    }

    /**
     * MOVE PIC X to PIC 9(n): digits are taken as-is; any non-digit byte contributes 0
     * (spaces / LOW-VALUES carry a zero numeric nibble on the mainframe).
     */
    public static long toUnsigned(String src, int digits) {
        String s = x(src, digits);
        long v = 0;
        for (int i = 0; i < digits; i++) {
            char c = s.charAt(i);
            v = v * 10 + (c >= '0' && c <= '9' ? c - '0' : 0);
        }
        return v;
    }

    /** COBOL {@code IS NUMERIC} class test for an unsigned DISPLAY / PIC X field. */
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

    public static boolean isAll(String s, char c) {
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

    /** Figurative ZEROS comparison on a PIC 9 DISPLAY field: every byte must be '0'. */
    public static boolean isZeros(String s) {
        return isAll(s, '0');
    }
}
