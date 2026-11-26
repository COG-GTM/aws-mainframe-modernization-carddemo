package com.carddemo.programs.cocrdslc;

/** Sequential cursor over a fixed-width record, one copybook field at a time. */
public final class FixedWidthReader {

    private final String record;
    private int pos;

    public FixedWidthReader(String record, int recordLength) {
        this.record = CobolText.slice(record, 0, recordLength);
    }

    public String x(int len) {
        String v = record.substring(pos, pos + len);
        pos += len;
        return v;
    }

    /** Unsigned PIC 9(len) DISPLAY item, read via IBM zoned-digit semantics. */
    public long n(int len) {
        return CobolText.zonedValue(x(len));
    }

    public void skip(int len) {
        pos += len;
    }
}
