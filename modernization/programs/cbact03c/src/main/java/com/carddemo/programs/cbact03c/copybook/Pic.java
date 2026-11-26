package com.carddemo.programs.cbact03c.copybook;

/** COBOL PICTURE helpers for DISPLAY-usage fields. */
public final class Pic {

    private Pic() {
    }

    /** MOVE to PIC X(n): left-justify, pad with spaces, truncate on the right. */
    public static String alphanumeric(String value, int length) {
        if (value.length() >= length) {
            return value.substring(0, length);
        }
        return value + " ".repeat(length - value.length());
    }

    /** MOVE to unsigned PIC 9(n): drop the sign and truncate high-order digits. */
    public static long unsignedTruncate(long value, int digits) {
        long mod = 1;
        for (int i = 0; i < digits; i++) {
            mod *= 10;
        }
        return Math.abs(value % mod);
    }

    public static String zeroPad(long value, int digits) {
        String s = Long.toString(value);
        return "0".repeat(digits - s.length()) + s;
    }

    public static boolean isNumeric(String s) {
        if (s.isEmpty()) {
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

    static long parseUnsigned(String s, String field) {
        if (!isNumeric(s)) {
            throw new IllegalArgumentException(field + " is not numeric: '" + s + "'");
        }
        return Long.parseLong(s);
    }
}
