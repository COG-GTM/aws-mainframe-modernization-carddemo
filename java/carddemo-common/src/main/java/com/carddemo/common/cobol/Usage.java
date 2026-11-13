package com.carddemo.common.cobol;

import java.util.Locale;
import java.util.Optional;

/** Storage usage of a COBOL elementary item. */
public enum Usage {
    DISPLAY,
    /** COMP, COMP-4, COMPUTATIONAL, BINARY. */
    BINARY,
    /** COMP-3, PACKED-DECIMAL. */
    PACKED_DECIMAL,
    /** COMP-5: native binary, not truncated to the picture. */
    NATIVE_BINARY;

    public static Optional<Usage> fromKeyword(String keyword) {
        return Optional.ofNullable(switch (keyword.toUpperCase(Locale.ROOT)) {
            case "DISPLAY" -> DISPLAY;
            case "COMP", "COMP-4", "COMPUTATIONAL", "COMPUTATIONAL-4", "BINARY" -> BINARY;
            case "COMP-3", "COMPUTATIONAL-3", "PACKED-DECIMAL" -> PACKED_DECIMAL;
            case "COMP-5", "COMPUTATIONAL-5" -> NATIVE_BINARY;
            case "COMP-1", "COMP-2", "COMPUTATIONAL-1", "COMPUTATIONAL-2" ->
                    throw new CobolDataException("Floating point usage " + keyword + " is not supported");
            default -> null;
        });
    }

    /** Bytes occupied by one occurrence of an elementary item with this usage. */
    public int storageLength(PicClause pic, SignMode signMode) {
        return switch (this) {
            case DISPLAY -> pic.isNumeric()
                    ? ZonedDecimal.byteLength(pic.digits(), pic.signed(), signMode)
                    : pic.displayLength();
            case BINARY, NATIVE_BINARY -> CobolBinary.byteLength(pic.digits());
            case PACKED_DECIMAL -> PackedDecimal.byteLength(pic.digits());
        };
    }
}
