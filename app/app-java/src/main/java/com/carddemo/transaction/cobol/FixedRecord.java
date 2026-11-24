package com.carddemo.transaction.cobol;

import java.math.BigDecimal;

/**
 * Read-only view over one fixed-length record. Offsets are 0-based; lengths are the PIC sizes.
 * Short lines (trailing FILLER stripped by the extract) are space-padded to the record length.
 */
public final class FixedRecord {

    private final String data;

    private FixedRecord(String data) {
        this.data = data;
    }

    public static FixedRecord of(String line, int recordLength) {
        String clean = line == null ? "" : line;
        if (clean.endsWith("\r")) {
            clean = clean.substring(0, clean.length() - 1);
        }
        return new FixedRecord(Pic.x(clean, recordLength));
    }

    public String raw() {
        return data;
    }

    /** PIC X(len). */
    public String x(int offset, int length) {
        return data.substring(offset, offset + length);
    }

    /** PIC 9(len). */
    public long n9(int offset, int length) {
        return ZonedDecimal.decodeUnsigned(x(offset, length));
    }

    /** PIC S9(i)V9(scale), total length = i + scale. */
    public BigDecimal s9(int offset, int length, int scale) {
        return ZonedDecimal.decode(x(offset, length), scale);
    }
}
