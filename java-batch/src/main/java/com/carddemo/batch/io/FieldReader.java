package com.carddemo.batch.io;

import java.math.BigDecimal;

/** Sequential cursor over one fixed-width record, reading COBOL display fields left to right. */
public final class FieldReader {

    private final String record;
    private int pos;

    /** Wraps {@code line}, right-padding it with spaces to {@code recordLength} (trimmed trailing FILLER). */
    public FieldReader(String line, int recordLength) {
        if (line.length() > recordLength) {
            throw new IllegalArgumentException(
                    "record is " + line.length() + " bytes, layout allows " + recordLength);
        }
        this.record = line.length() == recordLength ? line : line + " ".repeat(recordLength - line.length());
    }

    private String take(int length) {
        String s = record.substring(pos, pos + length);
        pos += length;
        return s;
    }

    /** {@code PIC X(n)}, kept exactly (including trailing spaces) so the record round-trips. */
    public String text(int length) {
        return take(length);
    }

    /** Unsigned {@code PIC 9(n)} with n &lt;= 18. */
    public long unsignedLong(int length) {
        String s = take(length);
        for (int i = 0; i < s.length(); i++) {
            if (!Character.isDigit(s.charAt(i))) {
                throw new IllegalArgumentException("non-digit in PIC 9(" + length + ") field '" + s + "'");
            }
        }
        return Long.parseLong(s);
    }

    /** Unsigned {@code PIC 9(n)} with n &lt;= 9. */
    public int unsignedInt(int length) {
        return Math.toIntExact(unsignedLong(length));
    }

    /** Signed {@code PIC S9(i)V9(scale)}: {@code length = i + scale} digits, sign overpunched. */
    public BigDecimal signedDecimal(int length, int scale) {
        return Overpunch.parse(take(length), scale);
    }

    /** Asserts the whole layout was consumed. */
    public void end() {
        if (pos != record.length()) {
            throw new IllegalStateException("layout consumed " + pos + " of " + record.length() + " bytes");
        }
    }
}
