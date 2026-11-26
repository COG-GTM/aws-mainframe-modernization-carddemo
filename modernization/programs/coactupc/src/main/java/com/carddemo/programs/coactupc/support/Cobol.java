package com.carddemo.programs.coactupc.support;

/**
 * Helpers that reproduce COBOL alphanumeric storage semantics on Java strings:
 * fixed-length fields, LOW-VALUES ({@code '\0'}), SPACES, MOVE padding/truncation,
 * space-padded comparisons and the intrinsic functions TRIM / UPPER-CASE / LOWER-CASE.
 */
public final class Cobol {

    public static final char LOW_VALUE = '\0';

    private Cobol() {
    }

    public static String spaces(int length) {
        return " ".repeat(length);
    }

    public static String lowValues(int length) {
        return String.valueOf(LOW_VALUE).repeat(length);
    }

    /** MOVE alphanumeric to PIC X(length): left-justify, pad with spaces or truncate on the right. */
    public static String pad(String value, int length) {
        String v = value == null ? "" : value;
        if (v.length() >= length) {
            return v.substring(0, length);
        }
        return v + spaces(length - v.length());
    }

    public static boolean isLowValues(String value) {
        for (int i = 0; i < value.length(); i++) {
            if (value.charAt(i) != LOW_VALUE) {
                return false;
            }
        }
        return true;
    }

    public static boolean isSpaces(String value) {
        for (int i = 0; i < value.length(); i++) {
            if (value.charAt(i) != ' ') {
                return false;
            }
        }
        return true;
    }

    public static boolean isLowValuesOrSpaces(String value) {
        return isLowValues(value) || isSpaces(value);
    }

    /** All characters are ZERO ('0'), i.e. {@code field = ZEROS} on an alphanumeric item. */
    public static boolean isZeros(String value) {
        for (int i = 0; i < value.length(); i++) {
            if (value.charAt(i) != '0') {
                return false;
            }
        }
        return !value.isEmpty();
    }

    /** Alphanumeric comparison: the shorter operand is padded on the right with spaces. */
    public static boolean alnumEquals(String a, String b) {
        int n = Math.max(a.length(), b.length());
        return pad(a, n).equals(pad(b, n));
    }

    /** Class test {@code IS NUMERIC} on an alphanumeric item: every character is 0-9. */
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

    /** FUNCTION TRIM: removes leading and trailing spaces only (LOW-VALUES are kept). */
    public static String trim(String value) {
        int start = 0;
        int end = value.length();
        while (start < end && value.charAt(start) == ' ') {
            start++;
        }
        while (end > start && value.charAt(end - 1) == ' ') {
            end--;
        }
        return value.substring(start, end);
    }

    /** FUNCTION UPPER-CASE: only a-z are changed. */
    public static String upper(String value) {
        StringBuilder sb = new StringBuilder(value.length());
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            sb.append(c >= 'a' && c <= 'z' ? (char) (c - 32) : c);
        }
        return sb.toString();
    }

    /** FUNCTION LOWER-CASE: only A-Z are changed. */
    public static String lower(String value) {
        StringBuilder sb = new StringBuilder(value.length());
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            sb.append(c >= 'A' && c <= 'Z' ? (char) (c + 32) : c);
        }
        return sb.toString();
    }

    /** INSPECT ... CONVERTING from TO spaces: every character found in {@code from} becomes a space. */
    public static String convertToSpaces(String value, String from) {
        StringBuilder sb = new StringBuilder(value.length());
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            sb.append(from.indexOf(c) >= 0 ? ' ' : c);
        }
        return sb.toString();
    }

    /** STRING ... DELIMITED BY SIZE INTO target, where the target is currently all spaces. */
    public static String stringInto(int targetLength, String... parts) {
        return pad(String.join("", parts), targetLength);
    }

    /**
     * Digit value of one byte of a zoned-decimal (DISPLAY) item, as the mainframe sees it:
     * the low-order nibble of the EBCDIC byte. Digits map to themselves; SPACE and LOW-VALUES
     * map to 0; overpunched sign characters ({, A-I, }, J-R) map to their digit.
     */
    public static int zonedDigit(char c) {
        if (c >= '0' && c <= '9') {
            return c - '0';
        }
        if (c == ' ' || c == LOW_VALUE || c == '{' || c == '}') {
            return 0;
        }
        if (c >= 'A' && c <= 'I') {
            return c - 'A' + 1;
        }
        if (c >= 'J' && c <= 'R') {
            return c - 'J' + 1;
        }
        if (c >= 'S' && c <= 'Z') {
            return c - 'S' + 2;
        }
        if (c >= 'a' && c <= 'i') {
            return c - 'a' + 1;
        }
        if (c >= 'j' && c <= 'r') {
            return c - 'j' + 1;
        }
        if (c >= 's' && c <= 'z') {
            return c - 's' + 2;
        }
        return (c & 0x0F) % 10;
    }

    /** Value of an unsigned PIC 9(n) DISPLAY item that REDEFINES / receives alphanumeric bytes. */
    public static long unsignedValue(String value) {
        long result = 0;
        for (int i = 0; i < value.length(); i++) {
            result = result * 10 + zonedDigit(value.charAt(i));
        }
        return result;
    }

    /** MOVE of an unsigned integer to PIC 9(digits): high-order truncation, zero fill. */
    public static String unsignedDisplay(long value, int digits) {
        long v = Math.abs(value);
        String s = Long.toString(v);
        if (s.length() > digits) {
            return s.substring(s.length() - digits);
        }
        return "0".repeat(digits - s.length()) + s;
    }

    /**
     * MOVE of a binary PIC S9(9) COMP item to an alphanumeric PIC X(length):
     * the sending item is treated as its 9-digit unsigned DISPLAY form, left-justified.
     */
    public static String binaryToAlnum(int value, int length) {
        return pad(unsignedDisplay(value, 9), length);
    }

    public static String replace(String target, int offset, String value) {
        return target.substring(0, offset) + value + target.substring(offset + value.length());
    }
}
