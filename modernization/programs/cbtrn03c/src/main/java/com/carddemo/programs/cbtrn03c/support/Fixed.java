package com.carddemo.programs.cbtrn03c.support;

/** Fixed-width alphanumeric helpers (PIC X semantics: left-justified, space-padded, right-truncated). */
public final class Fixed {
    private Fixed() {
    }

    public static String spaces(int length) {
        return " ".repeat(length);
    }

    /** MOVE alphanumeric to PIC X(length): pad with spaces or truncate on the right. */
    public static String pic(String value, int length) {
        String v = value == null ? "" : value;
        if (v.length() >= length) {
            return v.substring(0, length);
        }
        return v + spaces(length - v.length());
    }

    /** Reference modification safe against short records: value(offset+1:length), padded. */
    public static String slice(String record, int offset, int length) {
        if (record.length() >= offset + length) {
            return record.substring(offset, offset + length);
        }
        if (record.length() <= offset) {
            return spaces(length);
        }
        return pic(record.substring(offset), length);
    }

    /** MOVE unsigned integer to PIC 9(digits) DISPLAY: keep the low-order digits. */
    public static String unsigned(long value, int digits) {
        long mod = 1;
        for (int i = 0; i < digits; i++) {
            mod *= 10;
        }
        String s = Long.toString(Math.abs(value) % mod);
        return "0".repeat(digits - s.length()) + s;
    }

    /** Parse an unsigned PIC 9(n) DISPLAY field. */
    public static long parseUnsigned(String field, String name) {
        String t = field.trim();
        if (t.isEmpty()) {
            return 0L;
        }
        for (int i = 0; i < field.length(); i++) {
            char c = field.charAt(i);
            if (c != ' ' && (c < '0' || c > '9')) {
                throw new IllegalArgumentException(name + " is not numeric: '" + field + "'");
            }
        }
        return Long.parseLong(t);
    }

    /** COBOL class test NUMERIC on an alphanumeric item: every character is a digit. */
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
}
