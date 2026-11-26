package com.carddemo.programs.cbpaup0c;

import com.carddemo.programs.cbpaup0c.copybook.CobolCodec;

import java.math.BigDecimal;
import java.math.RoundingMode;

/** COBOL arithmetic, comparison and DISPLAY rules that CBPAUP0C relies on. */
public final class CobolSupport {

    private CobolSupport() {
    }

    /** Store into a signed integer PIC S9(n) without ON SIZE ERROR: high-order digits are lost, sign kept (TRUNC(STD)). */
    public static long truncateSigned(long value, int digits) {
        return value % pow10(digits);
    }

    /** Store into an unsigned PIC 9(n): sign is dropped and high-order digits are lost. */
    public static long truncateUnsigned(long value, int digits) {
        return Math.abs(value) % pow10(digits);
    }

    /** Store into PIC S9(i)V9(s): high-order integer digits are lost, extra decimals truncated. */
    public static BigDecimal truncateSigned(BigDecimal value, int integerDigits, int scale) {
        return value.setScale(scale, RoundingMode.DOWN).remainder(BigDecimal.TEN.pow(integerDigits));
    }

    /** {@code IS NUMERIC} on a PIC 9(n) DISPLAY field: every character is a digit 0-9. */
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

    /**
     * Relation condition between an unsigned integer PIC 9(n) DISPLAY item and a PIC X item. COBOL treats the
     * numeric operand as alphanumeric, pads the shorter operand with spaces, and compares using the program
     * collating sequence; EBCDIC (IBM037) is used here as on the mainframe.
     */
    public static int compareNumericToAlphanumeric(long unsignedValue, int digits, String alphanumeric) {
        String left = zeroPad(truncateUnsigned(unsignedValue, digits), digits);
        return compareAlphanumeric(left, alphanumeric);
    }

    public static int compareAlphanumeric(String a, String b) {
        int len = Math.max(a.length(), b.length());
        byte[] x = pad(a, len).getBytes(CobolCodec.EBCDIC);
        byte[] y = pad(b, len).getBytes(CobolCodec.EBCDIC);
        for (int i = 0; i < len; i++) {
            int d = (x[i] & 0xFF) - (y[i] & 0xFF);
            if (d != 0) {
                return d;
            }
        }
        return 0;
    }

    /** DISPLAY of a signed PIC S9(n) item: leading sign followed by n digits (GnuCOBOL rendering). */
    public static String displaySigned(long value, int digits) {
        long v = truncateSigned(value, digits);
        return (v < 0 ? "-" : "+") + zeroPad(Math.abs(v), digits);
    }

    /** DISPLAY of an unsigned PIC 9(n) item. */
    public static String displayUnsigned(long value, int digits) {
        return zeroPad(truncateUnsigned(value, digits), digits);
    }

    static String zeroPad(long value, int digits) {
        String s = Long.toString(value);
        return s.length() >= digits ? s : "0".repeat(digits - s.length()) + s;
    }

    private static String pad(String s, int len) {
        return s.length() >= len ? s : s + " ".repeat(len - s.length());
    }

    private static long pow10(int digits) {
        long p = 1;
        for (int i = 0; i < digits; i++) {
            p *= 10;
        }
        return p;
    }
}
