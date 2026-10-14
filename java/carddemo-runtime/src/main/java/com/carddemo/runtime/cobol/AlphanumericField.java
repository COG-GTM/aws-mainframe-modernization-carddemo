package com.carddemo.runtime.cobol;

/** PIC X(n) item (also used for alphanumeric views of group items). */
public final class AlphanumericField extends CobolField {

    public AlphanumericField(String name, int offset, int length) {
        super(name, offset, length);
    }

    public String get(CobolRecord record) {
        return raw(record);
    }

    /** MOVE value TO item: left-justified, space padded, truncated on the right. */
    public void set(CobolRecord record, String value) {
        record.setString(offset(), length(), value);
    }

    /** Reference modification {@code item(start:len)} read (1-based start, like COBOL). */
    public String substring(CobolRecord record, int start, int len) {
        return raw(record).substring(start - 1, start - 1 + len);
    }

    @Override
    public Object describe(CobolRecord record) {
        String v = raw(record);
        return PicX.isLowValues(v) ? null : v;
    }
}
