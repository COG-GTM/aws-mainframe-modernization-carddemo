package com.carddemo.programs.coadm01c.copybook;

/** COBOL MOVE semantics for fixed-width DISPLAY fields. LOW-VALUES are represented as {@code '\u0000'}. */
public final class CobolFields {

    public static final char LOW_VALUE = '\u0000';

    private CobolFields() {
    }

    /** MOVE to PIC X(len): left-justified, space padded, truncated on the right. {@code null} moves SPACES. */
    public static String alnum(String value, int len) {
        String v = value == null ? "" : value;
        if (v.length() >= len) {
            return v.substring(0, len);
        }
        return v + " ".repeat(len - v.length());
    }

    /** MOVE to PIC X(len) JUST RIGHT: right-justified, space padded, truncated on the left. */
    public static String justRight(String value, int len) {
        String v = value == null ? "" : value;
        if (v.length() >= len) {
            return v.substring(v.length() - len);
        }
        return " ".repeat(len - v.length()) + v;
    }

    /** MOVE to PIC 9(digits): high-order truncation, zero padded. */
    public static String unsigned(long value, int digits) {
        String s = Long.toString(Math.abs(value));
        if (s.length() >= digits) {
            return s.substring(s.length() - digits);
        }
        return "0".repeat(digits - s.length()) + s;
    }

    public static String spaces(int len) {
        return " ".repeat(len);
    }

    public static String lowValues(int len) {
        return String.valueOf(LOW_VALUE).repeat(len);
    }

    public static boolean isLowValues(String value) {
        return value != null && value.chars().allMatch(c -> c == LOW_VALUE);
    }

    /** {@code field = LOW-VALUES OR SPACES} (each figurative constant compared against the whole field). */
    public static boolean isLowValuesOrSpaces(String value) {
        return isLowValues(value) || value.chars().allMatch(c -> c == ' ');
    }

    /** Class test {@code IS NUMERIC} on an unsigned DISPLAY field: every byte is a digit 0-9. */
    public static boolean isNumeric(String value) {
        return value != null && !value.isEmpty() && value.chars().allMatch(c -> c >= '0' && c <= '9');
    }
}
