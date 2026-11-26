package com.carddemo.programs.cocrdslc;

import java.nio.charset.Charset;

/**
 * Helpers that reproduce IBM Enterprise COBOL semantics for DISPLAY (USAGE DISPLAY)
 * alphanumeric and zoned-decimal fields held as Java strings.
 * LOW-VALUES are represented by {@code '\u0000'}.
 */
public final class CobolText {

    public static final char LOW_VALUE = '\u0000';
    private static final Charset EBCDIC = Charset.forName("IBM037");

    private CobolText() {
    }

    /** MOVE alphanumeric to PIC X(len): left-justify, pad with spaces, truncate on the right. */
    public static String pic(String value, int len) {
        String v = value == null ? "" : value;
        if (v.length() >= len) {
            return v.substring(0, len);
        }
        return v + " ".repeat(len - v.length());
    }

    public static String spaces(int len) {
        return " ".repeat(len);
    }

    public static String lowValues(int len) {
        return String.valueOf(LOW_VALUE).repeat(len);
    }

    public static boolean isSpaces(String v) {
        return v.chars().allMatch(c -> c == ' ');
    }

    public static boolean isLowValues(String v) {
        return v.chars().allMatch(c -> c == LOW_VALUE);
    }

    /**
     * {@code numeric-display-item = ZERO(S)}: compared byte-for-byte against zero digits, as the
     * CardDemo runtime does for unsigned DISPLAY items, so "0" + spaces is not zero.
     */
    public static boolean isZeros(String v) {
        return !v.isEmpty() && v.chars().allMatch(c -> c == '0');
    }

    /** {@code IS NUMERIC} on a PIC X / unsigned PIC 9 DISPLAY item: every byte is 0-9. */
    public static boolean isNumeric(String v) {
        return !v.isEmpty() && v.chars().allMatch(c -> c >= '0' && c <= '9');
    }

    /**
     * Numeric value of an unsigned zoned-decimal (PIC 9(n) DISPLAY) item, using the EBCDIC
     * digit nibble of each byte as IBM hardware does when the item is packed for a numeric
     * comparison. A space (X'40') or LOW-VALUE (X'00') contributes 0. Returns -1 when any
     * byte has a digit nibble above 9 (an invalid digit: such a comparison never matches).
     */
    public static long zonedValue(String v) {
        long value = 0;
        for (int i = 0; i < v.length(); i++) {
            int nibble = digitNibble(v.charAt(i));
            if (nibble > 9) {
                return -1;
            }
            value = value * 10 + nibble;
        }
        return value;
    }

    private static int digitNibble(char c) {
        if (c >= '0' && c <= '9') {
            return c - '0';
        }
        if (c == LOW_VALUE) {
            return 0;
        }
        byte[] b = String.valueOf(c).getBytes(EBCDIC);
        return b.length == 0 ? 15 : b[0] & 0x0F;
    }

    /** MOVE unsigned integer to PIC 9(len) DISPLAY: keep the low-order digits, zero-fill. */
    public static String zoned(long value, int len) {
        String digits = Long.toString(Math.abs(value));
        if (digits.length() > len) {
            digits = digits.substring(digits.length() - len);
        }
        return "0".repeat(len - digits.length()) + digits;
    }

    /**
     * MOVE PIC S9(9) COMP to PIC X(len): the sending item is treated as an unsigned
     * 9-digit DISPLAY integer (the sign is lost), then moved alphanumerically.
     */
    public static String binaryS9_9ToAlpha(int value, int len) {
        return pic(zoned(value, 9), len);
    }

    /** Substring that pads with LOW-VALUES when the source is shorter (reference modification on a short COMMAREA). */
    public static String slice(String src, int offset, int len) {
        StringBuilder sb = new StringBuilder(len);
        for (int i = 0; i < len; i++) {
            int p = offset + i;
            sb.append(src != null && p < src.length() ? src.charAt(p) : LOW_VALUE);
        }
        return sb.toString();
    }
}
