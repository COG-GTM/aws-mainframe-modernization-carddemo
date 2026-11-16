package com.carddemo.legacyio;

import java.util.Arrays;
import java.util.HexFormat;

/**
 * Bytes of a numeric field that do not hold a valid value for its picture (for example LOW-VALUES or spaces in a
 * {@code PIC 9} field). Kept verbatim so the record still encodes back to the same bytes.
 */
public record InvalidValue(byte[] raw) {

    public InvalidValue {
        raw = raw.clone();
    }

    @Override
    public byte[] raw() {
        return raw.clone();
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof InvalidValue that && Arrays.equals(raw, that.raw);
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(raw);
    }

    @Override
    public String toString() {
        return "InvalidValue[" + HexFormat.of().formatHex(raw) + "]";
    }
}
