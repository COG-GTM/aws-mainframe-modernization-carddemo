package com.carddemo.transaction.cobol;

import java.math.BigDecimal;

/**
 * Walks a fixed-length record field by field in copybook order, so layouts are expressed with PIC
 * sizes only and offsets cannot drift.
 */
public final class FieldCursor {

    private final FixedRecord record;
    private int position;

    public FieldCursor(FixedRecord record) {
        this.record = record;
    }

    public static FieldCursor over(String line, int recordLength) {
        return new FieldCursor(FixedRecord.of(line, recordLength));
    }

    /** PIC X(length). */
    public String x(int length) {
        String v = record.x(position, length);
        position += length;
        return v;
    }

    /** PIC 9(digits). */
    public long n9(int digits) {
        long v = record.n9(position, digits);
        position += digits;
        return v;
    }

    /** PIC S9(integerDigits)V9(scale) DISPLAY, trailing overpunched sign. */
    public BigDecimal s9(int integerDigits, int scale) {
        BigDecimal v = record.s9(position, integerDigits + scale, scale);
        position += integerDigits + scale;
        return v;
    }

    /** FILLER. */
    public void filler(int length) {
        position += length;
    }

    public int position() {
        return position;
    }

    public FixedRecord record() {
        return record;
    }
}
