package com.carddemo.common.record;

import java.util.Locale;

/** COBOL {@code USAGE} of a numeric field. */
public enum Usage {
    /** Zoned decimal, one byte per digit, sign overpunched on the last byte. */
    DISPLAY,
    /** {@code COMP-3} / {@code PACKED-DECIMAL}: two digits per byte, sign in the last nibble. */
    PACKED,
    /** {@code COMP} / {@code COMP-4} / {@code BINARY}: big-endian binary, two's complement when signed. */
    BINARY;

    static Usage parse(String clause) {
        return switch (clause.toUpperCase(Locale.ROOT)) {
            case "DISPLAY" -> DISPLAY;
            case "COMP-3", "COMPUTATIONAL-3", "PACKED-DECIMAL" -> PACKED;
            case "COMP", "COMP-4", "COMPUTATIONAL", "COMPUTATIONAL-4", "BINARY" -> BINARY;
            default -> throw new IllegalArgumentException("Unsupported USAGE " + clause);
        };
    }
}
