package com.carddemo.modernization.records.codec;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CodingErrorAction;

/** Fixed-width text; decoding retains spaces. Unmappable text is rejected. */
public final class TextCodec {
    public static final Charset ASCII = Charset.forName("US-ASCII");
    public static final Charset EBCDIC = Charset.forName("IBM037");
    private TextCodec() {}

    public static byte[] encode(String value, int width, Charset charset) {
        if (width < 0 || value.length() > width) throw new IllegalArgumentException("Text overflow");
        try {
            ByteBuffer buffer = charset.newEncoder().onUnmappableCharacter(CodingErrorAction.REPORT)
                    .onMalformedInput(CodingErrorAction.REPORT).encode(CharBuffer.wrap(value + " ".repeat(width - value.length())));
            byte[] bytes = new byte[buffer.remaining()];
            buffer.get(bytes);
            if (bytes.length != width) throw new IllegalArgumentException("Not a single-byte field");
            return bytes;
        } catch (CharacterCodingException e) {
            throw new IllegalArgumentException("Unencodable text", e);
        }
    }

    public static String decode(byte[] bytes, Charset charset) {
        try {
            return charset.newDecoder().onMalformedInput(CodingErrorAction.REPORT)
                    .onUnmappableCharacter(CodingErrorAction.REPORT).decode(ByteBuffer.wrap(bytes)).toString();
        } catch (CharacterCodingException e) {
            throw new IllegalArgumentException("Invalid text", e);
        }
    }
}
