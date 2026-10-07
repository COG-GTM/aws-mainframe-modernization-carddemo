package com.carddemo.common.codec;

import java.util.Locale;
import java.util.Optional;

/** Storage format of an elementary item ({@code USAGE} clause). */
public enum Usage {
    /** Zoned decimal for numeric pictures, characters otherwise. */
    DISPLAY,
    /** {@code COMP}, {@code COMP-4}, {@code BINARY}. */
    BINARY,
    /** {@code COMP-3}, {@code PACKED-DECIMAL}. */
    PACKED;

    /** Maps a USAGE keyword; floating point and native binary are rejected (ADR-0004). */
    static Optional<Usage> fromKeyword(String keyword) {
        return switch (keyword.toUpperCase(Locale.ROOT)) {
            case "DISPLAY" -> Optional.of(DISPLAY);
            case "COMP", "COMPUTATIONAL", "COMP-4", "COMPUTATIONAL-4", "BINARY" -> Optional.of(BINARY);
            case "COMP-3", "COMPUTATIONAL-3", "PACKED-DECIMAL" -> Optional.of(PACKED);
            case "COMP-1", "COMPUTATIONAL-1", "COMP-2", "COMPUTATIONAL-2", "COMP-5", "COMPUTATIONAL-5",
                 "POINTER", "INDEX" -> throw new RecordFormatException("USAGE " + keyword + " is not supported");
            default -> Optional.empty();
        };
    }
}
