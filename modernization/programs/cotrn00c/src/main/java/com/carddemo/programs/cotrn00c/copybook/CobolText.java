package com.carddemo.programs.cotrn00c.copybook;

/** Fixed-width PIC X helpers with COBOL MOVE / figurative-constant semantics. */
public final class CobolText {

    public static final char LOW_VALUE = '\u0000';
    public static final char HIGH_VALUE = '\u00FF';

    private CobolText() {
    }

    /** MOVE alphanumeric to PIC X(len): left-justify, pad with spaces, truncate on the right. */
    public static String fit(String value, int len) {
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

    public static String highValues(int len) {
        return String.valueOf(HIGH_VALUE).repeat(len);
    }

    /** {@code X = SPACES}. */
    public static boolean isSpaces(String s) {
        return s != null && !s.isEmpty() && s.chars().allMatch(c -> c == ' ');
    }

    /** {@code X = LOW-VALUES}. */
    public static boolean isLowValues(String s) {
        return s != null && !s.isEmpty() && s.chars().allMatch(c -> c == LOW_VALUE);
    }

    /** {@code X = SPACES OR LOW-VALUES} (a mix of both is neither). */
    public static boolean isSpacesOrLowValues(String s) {
        return isSpaces(s) || isLowValues(s);
    }

    /** {@code X IS NUMERIC} on a PIC X field: every byte is 0-9 (spaces are not numeric). */
    public static boolean isNumeric(String s) {
        return s != null && !s.isEmpty() && s.chars().allMatch(c -> c >= '0' && c <= '9');
    }

    /** MOVE unsigned integer to PIC 9(len): high-order truncation, zero padded. */
    public static String unsignedDigits(long value, int len) {
        long mod = 1;
        for (int i = 0; i < len; i++) {
            mod *= 10;
        }
        long v = Math.floorMod(Math.abs(value), mod);
        String digits = Long.toString(v);
        return "0".repeat(len - digits.length()) + digits;
    }

    /** Read a PIC 9(n) DISPLAY field; non-numeric content (e.g. space padding) is read as 0. */
    public static long parseUnsigned(String s) {
        return isNumeric(s) ? Long.parseLong(s) : 0L;
    }
}
