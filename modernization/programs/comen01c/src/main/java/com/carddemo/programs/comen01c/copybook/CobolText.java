package com.carddemo.programs.comen01c.copybook;

/** Helpers reproducing COBOL alphanumeric MOVE / STRING semantics on fixed-width fields. */
public final class CobolText {

    public static final char LOW_VALUE = '\u0000';

    private CobolText() {
    }

    /** Alphanumeric MOVE: left-justify, pad with spaces, truncate on the right. */
    public static String move(String value, int size) {
        String v = value == null ? "" : value;
        if (v.length() >= size) {
            return v.substring(0, size);
        }
        return v + " ".repeat(size - v.length());
    }

    /** MOVE to a PIC X(n) JUST RIGHT field: right-justify, pad with spaces, truncate on the left. */
    public static String moveJustRight(String value, int size) {
        String v = value == null ? "" : value;
        if (v.length() >= size) {
            return v.substring(v.length() - size);
        }
        return " ".repeat(size - v.length()) + v;
    }

    public static String spaces(int size) {
        return " ".repeat(size);
    }

    public static String lowValues(int size) {
        return String.valueOf(LOW_VALUE).repeat(size);
    }

    /** Unsigned PIC 9(n) DISPLAY rendering (high-order truncation, zero padding). */
    public static String pic9(long value, int digits) {
        String s = Long.toString(Math.abs(value));
        if (s.length() > digits) {
            s = s.substring(s.length() - digits);
        }
        return "0".repeat(digits - s.length()) + s;
    }

    public static boolean isSpacesOrLowValues(String value) {
        return isAll(value, ' ') || isAll(value, LOW_VALUE);
    }

    public static boolean isAll(String value, char c) {
        for (int i = 0; i < value.length(); i++) {
            if (value.charAt(i) != c) {
                return false;
            }
        }
        return true;
    }

    /** COBOL class test {@code IS NUMERIC} for an unsigned DISPLAY item. */
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

    /** The part of {@code source} before the first occurrence of {@code delimiter} (STRING ... DELIMITED BY). */
    public static String delimitedBy(String source, String delimiter) {
        int idx = source.indexOf(delimiter);
        return idx < 0 ? source : source.substring(0, idx);
    }

    /**
     * STRING ... INTO target: writes the pieces from position 1 of the existing target value, stops on
     * overflow, and leaves the remaining target bytes unchanged.
     */
    public static String stringInto(String target, String... pieces) {
        StringBuilder sb = new StringBuilder(target);
        int pos = 0;
        for (String piece : pieces) {
            for (int i = 0; i < piece.length(); i++) {
                if (pos >= sb.length()) {
                    return sb.toString();
                }
                sb.setCharAt(pos++, piece.charAt(i));
            }
        }
        return sb.toString();
    }
}
