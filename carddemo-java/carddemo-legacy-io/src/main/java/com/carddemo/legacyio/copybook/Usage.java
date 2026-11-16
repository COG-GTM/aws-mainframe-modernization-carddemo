package com.carddemo.legacyio.copybook;

/** Storage format of an elementary item ({@code USAGE} clause). */
public enum Usage {
    /** One byte per character or digit; numerics are zoned decimal with the sign in the last byte. */
    DISPLAY,
    /** {@code COMP}, {@code COMP-4}, {@code COMP-5}, {@code BINARY}: big-endian two's complement. */
    BINARY,
    /** {@code COMP-3}, {@code PACKED-DECIMAL}: two digits per byte, sign in the last nibble. */
    PACKED_DECIMAL
}
