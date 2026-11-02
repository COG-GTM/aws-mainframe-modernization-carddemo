package com.carddemo.common.record;

/**
 * An elementary item (or group overlay) of a copybook record: its COBOL name, picture and byte
 * position inside the record.
 */
public abstract sealed class Field permits AlphanumericField, NumericField {

    private final String copybook;
    private final String name;
    private final String picture;
    private final int offset;
    private final int length;
    private final String view;

    Field(String copybook, String name, String picture, int offset, int length, String view) {
        this.copybook = copybook;
        this.name = name;
        this.picture = picture;
        this.offset = offset;
        this.length = length;
        this.view = view;
    }

    public String copybook() {
        return copybook;
    }

    /** COBOL data name, e.g. {@code ACCT-CURR-BAL}; occurrences carry their subscript, e.g. {@code EXP-CUST-ADDR-LINE(2)}. */
    public String name() {
        return name;
    }

    /** Picture and usage as declared in the copybook, e.g. {@code S9(10)V99 COMP-3}. */
    public String picture() {
        return picture;
    }

    /** Zero-based byte offset inside the record. */
    public int offset() {
        return offset;
    }

    /** Length in bytes. */
    public int length() {
        return length;
    }

    /**
     * Null for the record's primary storage map; otherwise the name of the {@code REDEFINES} view or
     * group item that overlays it.
     */
    public String view() {
        return view;
    }

    @Override
    public String toString() {
        return copybook + " " + name + " PIC " + picture + " @" + offset + "/" + length;
    }
}
