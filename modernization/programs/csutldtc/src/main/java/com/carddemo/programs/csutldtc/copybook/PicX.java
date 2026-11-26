package com.carddemo.programs.csutldtc.copybook;

/** Alphanumeric MOVE semantics for {@code PIC X(n)}: left-justify, space-pad or right-truncate. */
public final class PicX {
    private PicX() {
    }

    public static String of(String value, int length) {
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
