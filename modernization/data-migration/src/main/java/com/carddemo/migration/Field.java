package com.carddemo.migration;

/**
 * One elementary copybook item.
 *
 * @param column target column in the relational schema
 * @param offset zero-based byte offset in the record
 * @param length storage length in bytes (for PACKED this is (digits / 2) + 1)
 * @param scale  implied decimal places (the V in the PIC clause)
 */
public record Field(String copybookName, String column, int offset, int length, FieldType type, int scale) {

    public static Field alpha(String name, String column, int offset, int length) {
        return new Field(name, column, offset, length, FieldType.ALPHA, 0);
    }

    public static Field unsigned(String name, String column, int offset, int length) {
        return new Field(name, column, offset, length, FieldType.UNSIGNED_ZONED, 0);
    }

    public static Field signed(String name, String column, int offset, int length, int scale) {
        return new Field(name, column, offset, length, FieldType.SIGNED_ZONED, scale);
    }

    public static Field date(String name, String column, int offset) {
        return new Field(name, column, offset, 10, FieldType.DATE, 0);
    }

    public static Field timestamp(String name, String column, int offset) {
        return new Field(name, column, offset, 26, FieldType.TIMESTAMP, 0);
    }
}
