package com.carddemo.programs.cotrtupc.support;

import java.util.Arrays;

/** Fixed-width record storage (one char per COBOL byte), used for copybook layouts and COMMAREAs. */
public abstract class FixedRecord {

    protected final char[] data;

    protected FixedRecord(int length) {
        data = new char[length];
        Arrays.fill(data, CobolText.LOW_VALUE);
    }

    public int length() {
        return data.length;
    }

    protected String get(int offset, int len) {
        return new String(data, offset, len);
    }

    /** MOVE alphanumeric (pad with spaces / truncate). */
    protected void set(int offset, int len, String value) {
        String v = CobolText.pic(value, len);
        v.getChars(0, len, data, offset);
    }

    /** MOVE to unsigned zoned decimal PIC 9(len): high-order truncation. */
    protected void setNumeric(int offset, int len, long value) {
        String s = Long.toString(Math.abs(value));
        if (s.length() > len) {
            s = s.substring(s.length() - len);
        }
        set(offset, len, CobolText.repeat('0', len - s.length()) + s);
    }

    protected long getNumeric(int offset, int len) {
        String s = get(offset, len);
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) < '0' || s.charAt(i) > '9') {
                throw new IllegalStateException("Non-numeric data in PIC 9(" + len + "): '" + s + "'");
            }
        }
        return Long.parseLong(s);
    }

    protected void fill(int offset, int len, char c) {
        Arrays.fill(data, offset, offset + len, c);
    }

    public String toFixed() {
        return new String(data);
    }

    /** Group MOVE from a byte string; short sources are padded with LOW-VALUES (storage beyond EIBCALEN). */
    public void fromFixed(String s) {
        Arrays.fill(data, CobolText.LOW_VALUE);
        int n = Math.min(s.length(), data.length);
        s.getChars(0, n, data, 0);
    }
}
