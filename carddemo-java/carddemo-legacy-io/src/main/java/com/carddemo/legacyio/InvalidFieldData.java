package com.carddemo.legacyio;

import java.util.HexFormat;

/**
 * The value of a numeric field whose bytes are not valid for its picture (for example LOW-VALUES or spaces), kept
 * as-is so the record re-encodes unchanged, the way COBOL carries such data until a statement uses it.
 *
 * @param raw the field's bytes
 */
public record InvalidFieldData(byte[] raw) {

    public InvalidFieldData {
        raw = raw.clone();
    }

    @Override
    public byte[] raw() {
        return raw.clone();
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof InvalidFieldData that && java.util.Arrays.equals(raw, that.raw);
    }

    @Override
    public int hashCode() {
        return java.util.Arrays.hashCode(raw);
    }

    @Override
    public String toString() {
        return "InvalidFieldData[" + HexFormat.of().formatHex(raw) + "]";
    }
}
