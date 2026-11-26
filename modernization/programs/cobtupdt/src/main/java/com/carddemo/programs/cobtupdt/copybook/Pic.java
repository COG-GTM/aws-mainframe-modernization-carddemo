package com.carddemo.programs.cobtupdt.copybook;

/** Helpers for COBOL PIC X(n) MOVE semantics (left-justified, space-padded, right-truncated). */
public final class Pic {

    private Pic() {
    }

    public static String alphanumeric(String value, int length) {
        String v = value == null ? "" : value;
        if (v.length() >= length) {
            return v.substring(0, length);
        }
        return v + " ".repeat(length - v.length());
    }
}
