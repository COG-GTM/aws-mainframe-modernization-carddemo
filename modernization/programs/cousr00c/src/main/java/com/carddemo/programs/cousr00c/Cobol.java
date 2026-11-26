package com.carddemo.programs.cousr00c;

import java.nio.charset.Charset;
import java.util.Comparator;

/** COBOL MOVE / figurative-constant / collation semantics used by the port. */
public final class Cobol {

    public static final Charset EBCDIC = Charset.forName("IBM037");

    public static final char LOW_VALUE = '\u0000';

    /** HIGH-VALUES (X'FF') expressed in the EBCDIC code page so it collates highest. */
    public static final char HIGH_VALUE = new String(new byte[] {(byte) 0xFF}, EBCDIC).charAt(0);

    /** Mainframe (EBCDIC) collating sequence: VSAM KSDS keys are ordered by their EBCDIC bytes. */
    public static final Comparator<String> EBCDIC_ORDER = (a, b) -> {
        byte[] x = a.getBytes(EBCDIC);
        byte[] y = b.getBytes(EBCDIC);
        int n = Math.min(x.length, y.length);
        for (int i = 0; i < n; i++) {
            int c = Integer.compare(x[i] & 0xFF, y[i] & 0xFF);
            if (c != 0) {
                return c;
            }
        }
        return Integer.compare(x.length, y.length);
    };

    private Cobol() {
    }

    public static String spaces(int len) {
        return " ".repeat(len);
    }

    public static String lowValues(int len) {
        return String.valueOf(LOW_VALUE).repeat(len);
    }

    public static String highValues(int len) {
        return String.valueOf(HIGH_VALUE).repeat(len);
    }

    /** MOVE alphanumeric: left-justify, pad with spaces or truncate on the right. */
    public static String alnum(String value, int len) {
        String v = value == null ? "" : value;
        if (v.length() >= len) {
            return v.substring(0, len);
        }
        return v + spaces(len - v.length());
    }

    public static boolean isAll(String value, char c) {
        if (value == null || value.isEmpty()) {
            return false;
        }
        for (int i = 0; i < value.length(); i++) {
            if (value.charAt(i) != c) {
                return false;
            }
        }
        return true;
    }

    /** {@code field = SPACES OR LOW-VALUES}. A mix of spaces and low-values is neither. */
    public static boolean isSpacesOrLowValues(String value) {
        return isAll(value, ' ') || isAll(value, LOW_VALUE);
    }

    /** Unsigned PIC 9(n) DISPLAY: high-order truncation, zero filled. */
    public static String numericDisplay(long value, int digits) {
        long mod = 1;
        for (int i = 0; i < digits; i++) {
            mod *= 10;
        }
        long v = Math.abs(value) % mod;
        return String.format("%0" + digits + "d", v);
    }

    /** Unsigned PIC 9(n) arithmetic result: high-order digits are truncated. */
    public static long truncateUnsigned(long value, int digits) {
        return Long.parseLong(numericDisplay(value, digits));
    }

    /** Reads a PIC 9(n) DISPLAY field; non-numeric content (spaces, low-values) is read as zero. */
    public static long parseNumericDisplay(String value) {
        long result = 0;
        boolean any = false;
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            if (c < '0' || c > '9') {
                return 0;
            }
            result = result * 10 + (c - '0');
            any = true;
        }
        return any ? result : 0;
    }
}
