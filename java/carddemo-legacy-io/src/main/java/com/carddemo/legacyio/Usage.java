package com.carddemo.legacyio;

/** Storage format of an elementary item. */
public enum Usage {
    /** One character per digit; signed numbers carry the sign as an overpunch on the last digit. */
    DISPLAY,
    /** COMP, COMP-4, COMP-5, BINARY: big-endian two's complement in 2, 4 or 8 bytes. */
    BINARY,
    /** COMP-3, PACKED-DECIMAL: two digits per byte, sign in the last nibble. */
    PACKED_DECIMAL
}
