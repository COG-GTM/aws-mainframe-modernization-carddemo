package com.carddemo.common.online;

/** The edits COBOL programs apply to BMS input fields before using them. */
public final class ScreenInput {

    private ScreenInput() {
    }

    /**
     * {@code field = SPACES OR LOW-VALUES}: an absent/empty value, all spaces or all {@code X'00'}. A mix of the
     * two is neither figurative constant and therefore not blank, as in COBOL.
     */
    public static boolean isSpacesOrLowValues(String field) {
        if (field == null || field.isEmpty()) {
            return true;
        }
        return field.chars().allMatch(c -> c == ' ') || field.chars().allMatch(c -> c == 0);
    }

    /**
     * {@code FUNCTION UPPER-CASE}: only the letters {@code a-z} change (the single-byte code page of the region),
     * so the length never changes and no locale applies.
     */
    public static String upperCase(String field) {
        if (field == null) {
            return null;
        }
        char[] chars = field.toCharArray();
        for (int i = 0; i < chars.length; i++) {
            if (chars[i] >= 'a' && chars[i] <= 'z') {
                chars[i] = (char) (chars[i] - ('a' - 'A'));
            }
        }
        return new String(chars);
    }

    /** Drops trailing spaces: a {@code PIC X(n)} value without its padding (ADR-0003). */
    public static String rightTrim(String field) {
        if (field == null) {
            return null;
        }
        int end = field.length();
        while (end > 0 && field.charAt(end - 1) == ' ') {
            end--;
        }
        return field.substring(0, end);
    }
}
