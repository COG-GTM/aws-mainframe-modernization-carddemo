package com.carddemo.programs.cosgn00c.copybook;

/** Helpers for COBOL PIC X semantics: fixed width, space padding, LOW-VALUES. */
public final class CobolText {

    public static final char LOW_VALUE = '\u0000';

    private CobolText() {
    }

    /** MOVE alphanumeric to PIC X(len): left-justify, pad with spaces, truncate on the right. */
    public static String picX(String value, int len) {
        String v = value == null ? "" : value;
        if (v.length() >= len) {
            return v.substring(0, len);
        }
        return v + " ".repeat(len - v.length());
    }

    public static String spaces(int len) {
        return " ".repeat(len);
    }

    /** MOVE LOW-VALUES TO field. */
    public static String lowValues(int len) {
        return String.valueOf(LOW_VALUE).repeat(len);
    }

    /** {@code field = SPACES}: every byte is a space. */
    public static boolean isSpaces(String field) {
        return field != null && !field.isEmpty() && field.chars().allMatch(c -> c == ' ');
    }

    /** {@code field = LOW-VALUES}: every byte is X'00'. */
    public static boolean isLowValues(String field) {
        return field != null && !field.isEmpty() && field.chars().allMatch(c -> c == LOW_VALUE);
    }

    /** {@code field = SPACES OR LOW-VALUES}. */
    public static boolean isSpacesOrLowValues(String field) {
        return isSpaces(field) || isLowValues(field);
    }

    /** FUNCTION UPPER-CASE: folds only the single-byte letters a-z, every other byte is unchanged. */
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

    /** MOVE numeric to PIC 9(len) (unsigned, display): keeps the low-order digits. */
    public static String pic9(long value, int len) {
        long abs = Math.abs(value);
        String digits = Long.toString(abs);
        if (digits.length() > len) {
            digits = digits.substring(digits.length() - len);
        }
        return "0".repeat(len - digits.length()) + digits;
    }

    /** Reads a PIC 9(n) display field; non-numeric content is treated as zero. */
    public static long parse9(String field) {
        if (field == null || field.isBlank() || !field.chars().allMatch(Character::isDigit)) {
            return 0L;
        }
        return Long.parseLong(field);
    }
}
