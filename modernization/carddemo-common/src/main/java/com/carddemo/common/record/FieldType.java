package com.carddemo.common.record;

/** COBOL USAGE of a record field. */
public enum FieldType {
    /** {@code PIC X(n)}. */
    ALPHANUMERIC,
    /** {@code PIC [S]9(n)V9(s)} USAGE DISPLAY, sign overpunched on the last byte. */
    ZONED,
    /** {@code COMP-3}. */
    PACKED,
    /** {@code COMP} / {@code BINARY}. */
    BINARY,
    /** {@code FILLER}: padding only. */
    FILLER
}
