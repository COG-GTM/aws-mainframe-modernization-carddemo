package com.carddemo.programs.cbact01c.support;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

/**
 * Byte/character mapping for record buffers. Records are held as single-byte characters
 * (ISO-8859-1), matching the ASCII export in app/data/ASCII, so every byte survives a round trip.
 */
public final class RecordText {
    public static final Charset RECORD_CHARSET = StandardCharsets.ISO_8859_1;

    private RecordText() {
    }

    public static String toText(byte[] bytes) {
        return new String(bytes, RECORD_CHARSET);
    }

    public static byte[] toBytes(String text) {
        return text.getBytes(RECORD_CHARSET);
    }
}
