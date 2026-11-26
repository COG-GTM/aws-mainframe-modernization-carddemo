package com.carddemo.programs.csutldtc.copybook;

import java.nio.charset.StandardCharsets;

/**
 * LE varying string passed to CEEDAYS (WS-DATE-TO-TEST / WS-DATE-FORMAT):
 * <pre>
 *   02 Vstring-length  PIC S9(4) BINARY.
 *   02 Vstring-text.
 *      03 Vstring-char PIC X OCCURS 0 TO 256 TIMES DEPENDING ON Vstring-length.
 * </pre>
 */
public record VString(short length, String text) {
    public static final int MAX_LENGTH = 256;

    public VString {
        if (length < 0 || length > MAX_LENGTH) {
            throw new IllegalArgumentException("Vstring-length out of range 0..256: " + length);
        }
        text = PicX.of(text, length);
    }

    /** {@code MOVE LENGTH OF src TO Vstring-length; MOVE src TO Vstring-text}. */
    public static VString of(String fixedField) {
        return new VString((short) fixedField.length(), fixedField);
    }

    /**
     * Byte image of the whole group as used by a group MOVE: 2-byte big-endian
     * length followed by the ODO-bounded text (one byte per character).
     */
    public String groupImage() {
        char hi = (char) ((length >> 8) & 0xFF);
        char lo = (char) (length & 0xFF);
        return "" + hi + lo + text;
    }

    public byte[] groupBytes() {
        return groupImage().getBytes(StandardCharsets.ISO_8859_1);
    }
}
