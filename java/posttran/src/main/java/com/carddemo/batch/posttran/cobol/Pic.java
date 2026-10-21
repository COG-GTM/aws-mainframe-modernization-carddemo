package com.carddemo.batch.posttran.cobol;

/** Helpers for fixed-width {@code PIC X(n)} fields. */
public final class Pic {

    private Pic() {
    }

    /** MOVE alphanumeric: left-justify, pad with spaces or truncate on the right. */
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
}
