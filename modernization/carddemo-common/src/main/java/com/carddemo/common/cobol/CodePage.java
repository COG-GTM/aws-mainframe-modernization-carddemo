package com.carddemo.common.cobol;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;

/**
 * Character set of a record file. {@link #ASCII} is the line-sequential copy in {@code app/data/ASCII}
 * (7-bit text, read as ISO-8859-1 so every byte round-trips); {@link #CP037} is the EBCDIC fixed-block
 * copy in {@code app/data/EBCDIC}. Encoding a character the code page cannot represent fails instead
 * of writing a substitute byte.
 */
public enum CodePage {

    ASCII(StandardCharsets.ISO_8859_1),
    CP037(Charset.forName("IBM037"));

    private final Charset charset;
    private final byte space;

    CodePage(Charset charset) {
        this.charset = charset;
        this.space = " ".getBytes(charset)[0];
    }

    public Charset charset() {
        return charset;
    }

    /** The blank byte (X'20' in ASCII, X'40' in CP037), used to pad {@code PIC X} fields and records. */
    public byte space() {
        return space;
    }

    public String decode(byte[] bytes, int offset, int length) {
        return new String(bytes, offset, length, charset);
    }

    public String decode(byte[] bytes) {
        return decode(bytes, 0, bytes.length);
    }

    public byte[] encode(String text) {
        try {
            ByteBuffer buffer = charset.newEncoder()
                    .onMalformedInput(CodingErrorAction.REPORT)
                    .onUnmappableCharacter(CodingErrorAction.REPORT)
                    .encode(CharBuffer.wrap(text));
            byte[] bytes = new byte[buffer.remaining()];
            buffer.get(bytes);
            return bytes;
        } catch (CharacterCodingException ex) {
            throw new IllegalArgumentException("Text not representable in " + this + ": " + text, ex);
        }
    }

    /** Converts a record image from this code page to {@code target}, character by character. */
    public byte[] convert(byte[] bytes, CodePage target) {
        return target == this ? bytes.clone() : target.encode(decode(bytes));
    }

    /** Parses {@code ASCII} or {@code CP037} (case-insensitive; {@code EBCDIC} is accepted for CP037). */
    public static CodePage parse(String name) {
        String upper = name.trim().toUpperCase(java.util.Locale.ROOT);
        return switch (upper) {
            case "ASCII" -> ASCII;
            case "CP037", "EBCDIC", "IBM037" -> CP037;
            default -> throw new IllegalArgumentException("Unknown code page '" + name + "' (ASCII or CP037)");
        };
    }
}
