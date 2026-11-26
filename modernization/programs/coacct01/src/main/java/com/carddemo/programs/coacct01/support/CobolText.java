package com.carddemo.programs.coacct01.support;

/** Alphanumeric (PIC X) MOVE / STRING helpers with COBOL padding and truncation rules. */
public final class CobolText {

    private CobolText() {
    }

    /** MOVE to PIC X(length): left-justified, space-padded, truncated on the right. */
    public static String pic(String value, int length) {
        String v = value == null ? "" : value;
        if (v.length() >= length) {
            return v.substring(0, length);
        }
        return v + " ".repeat(length - v.length());
    }

    public static String spaces(int length) {
        return " ".repeat(length);
    }

    /**
     * STRING ... DELIMITED BY SIZE INTO target: writes the parts from position 1, leaves the
     * rest of the receiving field untouched and stops silently on overflow.
     */
    public static String stringInto(String target, String... parts) {
        StringBuilder sb = new StringBuilder(target);
        int pos = 0;
        for (String part : parts) {
            for (int i = 0; i < part.length() && pos < sb.length(); i++) {
                sb.setCharAt(pos++, part.charAt(i));
            }
        }
        return sb.toString();
    }

    /** MOVE of a binary/signed integer to an unsigned PIC 9(digits) DISPLAY field. */
    public static long unsignedDisplay(long value, int digits) {
        return Math.floorMod(Math.abs(value), pow10(digits));
    }

    /** Unsigned PIC 9(digits) DISPLAY rendering (zero-padded, high-order truncation). */
    public static String digits(long value, int digits) {
        String s = Long.toString(unsignedDisplay(value, digits));
        return "0".repeat(digits - s.length()) + s;
    }

    static long pow10(int n) {
        long r = 1;
        for (int i = 0; i < n; i++) {
            r *= 10;
        }
        return r;
    }
}
