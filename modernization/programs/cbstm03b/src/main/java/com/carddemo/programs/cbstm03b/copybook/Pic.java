package com.carddemo.programs.cbstm03b.copybook;

/** COBOL MOVE helpers for DISPLAY (character) fields. */
public final class Pic {

    private Pic() {
    }

    /** MOVE to PIC X(n): left-justified, space-padded or truncated on the right. */
    public static String alphanumeric(String value, int length) {
        String v = value == null ? "" : value;
        if (v.length() >= length) {
            return v.substring(0, length);
        }
        return v + " ".repeat(length - v.length());
    }

    /**
     * MOVE of an alphanumeric sender to unsigned PIC 9(n) DISPLAY: the sender is treated as an
     * integer, so it is right-justified, zero-filled on the left or truncated on the left.
     * Characters are copied as-is (non-digit content is not validated, as in COBOL).
     */
    public static String unsignedNumericFromAlphanumeric(String value, int length) {
        String v = value == null ? "" : value;
        if (v.length() >= length) {
            return v.substring(v.length() - length);
        }
        return "0".repeat(length - v.length()) + v;
    }

    /** Slice of a fixed-width record (1-based COBOL offset). */
    static String field(String record, int offset1, int length) {
        return record.substring(offset1 - 1, offset1 - 1 + length);
    }

    static String requireLength(String record, int length, String layout) {
        if (record == null || record.length() != length) {
            throw new IllegalArgumentException(layout + " record must be " + length + " bytes, got "
                    + (record == null ? "null" : record.length()));
        }
        return record;
    }
}
