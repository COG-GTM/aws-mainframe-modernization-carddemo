package com.carddemo.recordio.codec;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

/**
 * Character encoding of a fixed-width dataset image.
 *
 * <p>The shipped CardDemo datasets under {@code app/data/EBCDIC} are IBM-037 (US EBCDIC); the
 * {@code app/data/ASCII} copies are byte-for-byte translations, so the zoned-decimal sign
 * overpunch survives as the characters {@code {A-I} (positive) and {@code }J-R} (negative) in both.
 */
public record RecordEncoding(Charset charset) {

    public static final RecordEncoding EBCDIC = new RecordEncoding(Charset.forName("IBM037"));
    public static final RecordEncoding ASCII = new RecordEncoding(StandardCharsets.US_ASCII);

    public static RecordEncoding of(String name) {
        return switch (name.trim().toUpperCase()) {
            case "EBCDIC", "IBM037", "CP037" -> EBCDIC;
            case "ASCII", "US-ASCII" -> ASCII;
            default -> new RecordEncoding(Charset.forName(name));
        };
    }

    public byte space() {
        return " ".getBytes(charset)[0];
    }
}
