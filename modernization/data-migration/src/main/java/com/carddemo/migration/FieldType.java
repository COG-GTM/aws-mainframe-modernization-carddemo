package com.carddemo.migration;

/** COBOL storage formats found in the CardDemo copybooks. */
public enum FieldType {
    /** PIC X(n): character data, trailing spaces trimmed. */
    ALPHA,
    /** PIC 9(n) DISPLAY: unsigned zoned decimal. */
    UNSIGNED_ZONED,
    /** PIC S9(n)V9(m) DISPLAY: signed zoned decimal, sign overpunched in the last byte. */
    SIGNED_ZONED,
    /** PIC S9(n)V9(m) COMP-3: packed decimal, two digits per byte, sign in the low nibble of the last byte. */
    PACKED,
    /** PIC X(10) holding YYYY-MM-DD. */
    DATE,
    /** PIC X(26) holding YYYY-MM-DD-HH.MM.SS.ffffff or YYYY-MM-DD HH:MM:SS.ffffff. */
    TIMESTAMP
}
