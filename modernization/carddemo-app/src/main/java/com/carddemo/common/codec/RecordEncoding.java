package com.carddemo.common.codec;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Locale;
import java.util.Objects;

/**
 * Single-byte character set of a record image.
 *
 * <p>The shipped datasets under {@code app/data/EBCDIC} are IBM-037; {@code app/data/ASCII} holds
 * translated copies. IBM-037 maps the zoned-decimal sign zones C and D to the characters
 * {@code {A-I} and {@code }J-R}, so zoned-decimal overpunch is handled at character level and works
 * for both encodings. Binary and packed fields are never translated: they are read from the raw image.
 */
public record RecordEncoding(Charset charset) {

    public static final RecordEncoding EBCDIC = new RecordEncoding(Charset.forName("IBM037"));
    public static final RecordEncoding ASCII = new RecordEncoding(StandardCharsets.ISO_8859_1);

    public RecordEncoding {
        Objects.requireNonNull(charset, "charset");
        if (charset.newEncoder().maxBytesPerChar() != 1.0f) {
            throw new IllegalArgumentException("record images need a single-byte charset, got " + charset);
        }
    }

    public static RecordEncoding of(String name) {
        return switch (name.trim().toUpperCase(Locale.ROOT)) {
            case "EBCDIC", "IBM037", "IBM-037", "CP037" -> EBCDIC;
            case "ASCII", "US-ASCII", "ISO-8859-1", "LATIN-1" -> ASCII;
            default -> new RecordEncoding(Charset.forName(name));
        };
    }

    public byte space() {
        return encode(" ")[0];
    }

    public String decode(byte[] image, int offset, int length) {
        return new String(image, offset, length, charset);
    }

    public byte[] encode(String text) {
        try {
            ByteBuffer encoded = charset.newEncoder().encode(CharBuffer.wrap(text));
            byte[] bytes = new byte[encoded.remaining()];
            encoded.get(bytes);
            return bytes;
        } catch (CharacterCodingException e) {
            throw new RecordFormatException("text is not representable in " + charset + ": '" + text + "'", e);
        }
    }
}
