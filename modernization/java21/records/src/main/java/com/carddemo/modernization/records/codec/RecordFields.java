package com.carddemo.modernization.records.codec;

import java.io.ByteArrayOutputStream;
import java.nio.charset.Charset;
import java.util.Arrays;

/** Sequential fixed-width field reader/writer. Instances are local to a codec call. */
public final class RecordFields {
    private final byte[] input;
    private final Charset charset;
    private final ByteArrayOutputStream output = new ByteArrayOutputStream();
    private int offset;
    public RecordFields(byte[] input, int length, Charset charset) {
        if (input != null && input.length != length) throw new IllegalArgumentException("Wrong record length: expected " + length);
        this.input = input; this.charset = charset;
    }
    private byte[] take(int width) {
        byte[] bytes = Arrays.copyOfRange(input, offset, offset + width);
        offset += width;
        return bytes;
    }
    public String text(int width) { return TextCodec.decode(take(width), charset); }
    public String unsigned(int width) { return ZonedDecimalCodec.unsigned(text(width), width); }
    public ZonedDecimal money(int width) { return new ZonedDecimalCodec(width, 2).decode(take(width), charset); }
    public void text(String value, int width) { output.writeBytes(TextCodec.encode(value, width, charset)); }
    public void unsigned(String value, int width) { text(ZonedDecimalCodec.unsigned(value, width), width); }
    public void money(ZonedDecimal value, int width) { output.writeBytes(new ZonedDecimalCodec(width, 2).encode(value, charset)); }
    public byte[] bytes() { return output.toByteArray(); }
}
