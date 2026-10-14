package com.carddemo.runtime.cobol;

/** Descriptor of an item at a fixed offset/length inside a {@link CobolRecord}. */
public abstract sealed class CobolField permits AlphanumericField, NumericField {

    private final String name;
    private final int offset;
    private final int length;

    protected CobolField(String name, int offset, int length) {
        this.name = name;
        this.offset = offset;
        this.length = length;
    }

    public final String name() {
        return name;
    }

    public final int offset() {
        return offset;
    }

    public final int length() {
        return length;
    }

    /** Raw content as alphanumeric (reference modification of the whole item). */
    public final String raw(CobolRecord record) {
        return record.getString(offset, length);
    }

    /** Raw byte content. */
    public final byte[] rawBytes(CobolRecord record) {
        return record.getBytes(offset, length);
    }

    /** MOVE LOW-VALUES / SPACES / HIGH-VALUES TO item (byte fill, valid for any usage). */
    public final void fill(CobolRecord record, char c) {
        record.setString(offset, length, PicX.repeat(c, length));
    }

    public final boolean isLowValues(CobolRecord record) {
        return PicX.isLowValues(raw(record));
    }

    /** Display value used by {@link RecordLayout#describe}: null when the item is all LOW-VALUES. */
    public abstract Object describe(CobolRecord record);

    @Override
    public String toString() {
        return name + "@" + offset + "(" + length + ")";
    }
}
