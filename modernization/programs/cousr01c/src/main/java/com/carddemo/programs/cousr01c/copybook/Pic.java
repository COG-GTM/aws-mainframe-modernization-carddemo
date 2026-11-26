package com.carddemo.programs.cousr01c.copybook;

/** COBOL PICTURE-clause move semantics used by the copybook classes. */
public final class Pic {

    public static final char LOW_VALUE = '\u0000';
    public static final char SPACE = ' ';

    private Pic() {
    }

    /** MOVE alphanumeric to PIC X(len): left-justify, space-pad or right-truncate. Null moves SPACES. */
    public static String x(String value, int len) {
        if (value == null) {
            return spaces(len);
        }
        if (value.length() >= len) {
            return value.substring(0, len);
        }
        return value + spaces(len - value.length());
    }

    /** MOVE numeric to unsigned PIC 9(digits) DISPLAY: drops sign and high-order digits, zero-fills. */
    public static String nine(long value, int digits) {
        String s = Long.toString(Math.abs(value));
        if (s.length() > digits) {
            s = s.substring(s.length() - digits);
        }
        return "0".repeat(digits - s.length()) + s;
    }

    public static String spaces(int len) {
        return " ".repeat(len);
    }

    public static String lowValues(int len) {
        return String.valueOf(LOW_VALUE).repeat(len);
    }

    public static boolean isSpaces(String v) {
        return v.chars().allMatch(c -> c == SPACE);
    }

    public static boolean isLowValues(String v) {
        return v.chars().allMatch(c -> c == LOW_VALUE);
    }

    /** {@code field = SPACES OR LOW-VALUES}: the whole field is spaces, or the whole field is low-values. */
    public static boolean isSpacesOrLowValues(String v) {
        return isSpaces(v) || isLowValues(v);
    }

    /** {@code PIC 9 IS NUMERIC}. */
    public static boolean isNumeric(String v) {
        return !v.isEmpty() && v.chars().allMatch(c -> c >= '0' && c <= '9');
    }

    /** Text as a terminal would show it: low-values blank, trailing blanks removed. */
    public static String display(String v) {
        return v.replace(LOW_VALUE, SPACE).stripTrailing();
    }
}
