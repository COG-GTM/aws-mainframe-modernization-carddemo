package com.carddemo.programs.cbact01c.support;

/** PIC X helpers: alphanumeric MOVE semantics (left-justify, pad with spaces, truncate on the right). */
public final class Pic {
    private Pic() {
    }

    public static String x(String value, int length) {
        String v = value == null ? "" : value;
        if (v.length() >= length) {
            return v.substring(0, length);
        }
        return v + " ".repeat(length - v.length());
    }

    public static String spaces(int length) {
        return " ".repeat(length);
    }

    /** COBOL class test {@code IS NUMERIC} on an alphanumeric item: every character is 0-9. */
    public static boolean isNumeric(String value) {
        if (value.isEmpty()) {
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
