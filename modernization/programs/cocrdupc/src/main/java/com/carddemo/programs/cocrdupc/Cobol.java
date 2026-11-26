package com.carddemo.programs.cocrdupc;

/** COBOL data semantics for fixed-length DISPLAY fields (PIC X / unsigned PIC 9). */
public final class Cobol {

    public static final char LOW_VALUE = '\u0000';

    private Cobol() {
    }

    public static String spaces(int len) {
        return " ".repeat(len);
    }

    public static String lowValues(int len) {
        return String.valueOf(LOW_VALUE).repeat(len);
    }

    public static String zeros(int len) {
        return "0".repeat(len);
    }

    /** MOVE alphanumeric TO PIC X(len): left-justify, pad with spaces, truncate on the right. */
    public static String alnum(String value, int len) {
        String v = value == null ? "" : value;
        return v.length() >= len ? v.substring(0, len) : v + spaces(len - v.length());
    }

    /** BMS JUSTIFY=(RIGHT,BLANK) input padding. */
    public static String rightJustify(String value, int len) {
        String v = value == null ? "" : value;
        return v.length() >= len ? v.substring(0, len) : spaces(len - v.length()) + v;
    }

    /** MOVE numeric TO PIC 9(digits): sign dropped, high-order digits truncated, zero filled. */
    public static String numeric(long value, int digits) {
        String s = Long.toString(Math.abs(value));
        if (s.length() > digits) {
            s = s.substring(s.length() - digits);
        }
        return zeros(digits - s.length()) + s;
    }

    public static boolean isLowValues(String s) {
        return isAll(s, LOW_VALUE);
    }

    public static boolean isSpaces(String s) {
        return isAll(s, ' ');
    }

    /** Alphanumeric comparison with figurative ZEROS. */
    public static boolean isZeros(String s) {
        return isAll(s, '0');
    }

    /** Class test IS NUMERIC on an unsigned DISPLAY / alphanumeric field. */
    public static boolean isNumeric(String s) {
        if (s == null || s.isEmpty()) {
            return false;
        }
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c < '0' || c > '9') {
                return false;
            }
        }
        return true;
    }

    /**
     * Numeric value of a zoned DISPLAY field as the mainframe PACK instruction sees it: each byte
     * contributes its low-order nibble. Digits, spaces and LOW-VALUES are exact; other bytes are
     * reduced modulo 10 (the mainframe would either accept the nibble or raise S0C7).
     */
    public static long displayValue(String s) {
        long v = 0;
        for (int i = 0; i < s.length(); i++) {
            v = v * 10 + ((s.charAt(i) & 0x0F) % 10);
        }
        return v;
    }

    /** Numeric comparison of a zoned DISPLAY redefinition against ZERO. */
    public static boolean displayIsZero(String s) {
        for (int i = 0; i < s.length(); i++) {
            if ((s.charAt(i) & 0x0F) != 0) {
                return false;
            }
        }
        return true;
    }

    /** INSPECT ... CONVERTING 'a..z' TO 'A..Z' and FUNCTION UPPER-CASE (single-byte, A-Z only). */
    public static String upper(String s) {
        StringBuilder sb = new StringBuilder(s.length());
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            sb.append(c >= 'a' && c <= 'z' ? (char) (c - 32) : c);
        }
        return sb.toString();
    }

    /** Alphanumeric comparison: the shorter operand is padded with spaces. */
    public static boolean eq(String a, String b) {
        int len = Math.max(a.length(), b.length());
        return alnum(a, len).equals(alnum(b, len));
    }

    private static boolean isAll(String s, char c) {
        if (s == null || s.isEmpty()) {
            return false;
        }
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) != c) {
                return false;
            }
        }
        return true;
    }
}
