package com.carddemo.programs.cbact02c.copybook;

/** COBOL MOVE semantics for DISPLAY (zoned/alphanumeric) fields. */
public final class Pic {

    private Pic() {
    }

    /** MOVE to PIC X(n): left-justify, pad with spaces, truncate on the right. */
    public static String alpha(String value, int length) {
        String v = value == null ? "" : value;
        if (v.length() >= length) {
            return v.substring(0, length);
        }
        return v + " ".repeat(length - v.length());
    }

    /** MOVE to unsigned PIC 9(n): keep the low-order n digits, zero-fill on the left. */
    public static String unsignedNumeric(long value, int length) {
        if (value < 0) {
            throw new IllegalArgumentException("unsigned PIC 9(" + length + ") cannot hold " + value);
        }
        String digits = Long.toString(value);
        if (digits.length() > length) {
            return digits.substring(digits.length() - length);
        }
        return "0".repeat(length - digits.length()) + digits;
    }

    /** IF field IS NUMERIC for an unsigned zoned/alphanumeric field: every byte is 0-9. */
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
}
