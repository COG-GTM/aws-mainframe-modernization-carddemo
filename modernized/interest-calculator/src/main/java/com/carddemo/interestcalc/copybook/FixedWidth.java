package com.carddemo.interestcalc.copybook;

/** Helpers for slicing and building fixed-width copybook records. */
public final class FixedWidth {

    private FixedWidth() {
    }

    /** Returns the {@code length} bytes of {@code record} starting at zero-based {@code offset}. */
    public static String field(String record, int offset, int length) {
        return record.substring(offset, offset + length);
    }

    /** {@code PIC X(n)} - left justified, space filled. */
    public static String alphanumeric(String value, int length) {
        if (value.length() > length) {
            throw new IllegalArgumentException("'" + value + "' does not fit in PIC X(" + length + ")");
        }
        return value + " ".repeat(length - value.length());
    }

    /** {@code PIC 9(n)} - right justified, zero filled, unsigned. */
    public static String unsigned(String digits, int length) {
        String trimmed = digits.trim();
        if (trimmed.length() > length) {
            throw new IllegalArgumentException("'" + digits + "' does not fit in PIC 9(" + length + ")");
        }
        return "0".repeat(length - trimmed.length()) + trimmed;
    }

    /** Normalises a raw dataset line to exactly {@code length} bytes (strip CR, pad with spaces). */
    public static String record(String line, int length) {
        String stripped = line.endsWith("\r") ? line.substring(0, line.length() - 1) : line;
        if (stripped.length() > length) {
            return stripped.substring(0, length);
        }
        return stripped + " ".repeat(length - stripped.length());
    }
}
