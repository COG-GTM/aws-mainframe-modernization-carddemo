package com.carddemo.programs.cousr02c.copybook;

/** Fixed-width alphanumeric (PIC X) helpers reproducing COBOL MOVE / figurative-constant semantics. */
public final class Pic {

    public static final char LOW_VALUE = '\u0000';

    private Pic() {
    }

    /** MOVE alphanumeric: left-justify, truncate on the right, pad with spaces. A null source is treated as spaces. */
    public static String x(String value, int size) {
        String v = value == null ? "" : value;
        if (v.length() >= size) {
            return v.substring(0, size);
        }
        return v + " ".repeat(size - v.length());
    }

    public static String spaces(int size) {
        return " ".repeat(size);
    }

    public static String lowValues(int size) {
        return String.valueOf(LOW_VALUE).repeat(size);
    }

    /** {@code field = SPACES}: every byte is a space. */
    public static boolean isSpaces(String field) {
        return field != null && field.chars().allMatch(c -> c == ' ');
    }

    /** {@code field = LOW-VALUES}: every byte is X'00'. */
    public static boolean isLowValues(String field) {
        return field != null && field.chars().allMatch(c -> c == LOW_VALUE);
    }

    /**
     * {@code field = SPACES OR LOW-VALUES}. A field mixing spaces and low-values is neither, exactly as in COBOL.
     */
    public static boolean isSpacesOrLowValues(String field) {
        return isSpaces(field) || isLowValues(field);
    }

    /** Characters of {@code value} up to (excluding) the first space: STRING ... DELIMITED BY SPACE. */
    public static String delimitedBySpace(String value) {
        int idx = value.indexOf(' ');
        return idx < 0 ? value : value.substring(0, idx);
    }

    /** Right-trims spaces only (convenience for display/tests; does not touch low-values). */
    public static String trimRight(String value) {
        int end = value.length();
        while (end > 0 && value.charAt(end - 1) == ' ') {
            end--;
        }
        return value.substring(0, end);
    }
}
