package com.carddemo.common.record;

/**
 * One elementary item of a copybook: name, 0-based {@code offset}, byte {@code length}, usage and,
 * for numerics, the picture's integer digits, scale and sign.
 */
public record Field(String name, int offset, int length, FieldType type, int integerDigits, int scale, boolean signed) {

    public Field {
        if (offset < 0 || length < 1) {
            throw new IllegalArgumentException("Bad offset/length for " + name + ": " + offset + "/" + length);
        }
    }

    /** 1-based start position, as copybook comments and JCL SORT cards count. */
    public int position() {
        return offset + 1;
    }

    public int end() {
        return offset + length;
    }

    public boolean numeric() {
        return type == FieldType.ZONED || type == FieldType.PACKED || type == FieldType.BINARY;
    }
}
