package com.carddemo.batch.io;

import java.math.BigDecimal;

/** Builds one fixed-width record from COBOL display fields, left to right. */
public final class FieldWriter {

    private final int recordLength;
    private final StringBuilder sb;

    public FieldWriter(int recordLength) {
        this.recordLength = recordLength;
        this.sb = new StringBuilder(recordLength);
    }

    /** {@code PIC X(n)}: space-padded on the right; longer values are rejected. */
    public FieldWriter text(String value, int length) {
        String v = value == null ? "" : value;
        if (v.length() > length) {
            throw new IllegalArgumentException("'" + v + "' does not fit in PIC X(" + length + ")");
        }
        sb.append(v).append(" ".repeat(length - v.length()));
        return this;
    }

    /** Unsigned {@code PIC 9(n)}: zero-padded on the left. */
    public FieldWriter unsigned(long value, int length) {
        if (value < 0) {
            throw new IllegalArgumentException("negative value " + value + " for unsigned PIC 9(" + length + ")");
        }
        String digits = Long.toString(value);
        if (digits.length() > length) {
            throw new IllegalArgumentException(value + " does not fit in PIC 9(" + length + ")");
        }
        sb.append("0".repeat(length - digits.length())).append(digits);
        return this;
    }

    /** Signed {@code PIC S9(i)V9(scale)} with the sign overpunched on the last digit. */
    public FieldWriter signedDecimal(BigDecimal value, int length, int scale) {
        sb.append(Overpunch.format(value, length, scale));
        return this;
    }

    @Override
    public String toString() {
        if (sb.length() != recordLength) {
            throw new IllegalStateException("record is " + sb.length() + " bytes, layout is " + recordLength);
        }
        return sb.toString();
    }
}
