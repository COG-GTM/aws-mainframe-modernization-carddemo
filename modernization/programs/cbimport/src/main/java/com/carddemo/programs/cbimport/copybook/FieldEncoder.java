package com.carddemo.programs.cbimport.copybook;

import java.io.ByteArrayOutputStream;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;

/** Sequential encoder that builds a byte record laid out per a copybook (inverse of {@link FieldCursor}). */
public final class FieldEncoder {

    private final ByteArrayOutputStream out = new ByteArrayOutputStream();
    private final Charset charset;

    public FieldEncoder(Charset charset) {
        this.charset = charset;
    }

    public FieldEncoder alnum(String value, int len) {
        String text = Pic.alnum(value, len);
        byte[] bytes = text.getBytes(charset);
        if (EbcdicNewlines.collapses(charset) && bytes.length == text.length()) {
            EbcdicNewlines.restoreLineFeeds(text, ByteBuffer.wrap(bytes));
        }
        out.writeBytes(bytes);
        return this;
    }

    public FieldEncoder zonedUnsigned(long value, int digits) {
        return alnum(Pic.displayUnsigned(value, digits), digits);
    }

    public FieldEncoder zonedSigned(BigDecimal value, int intDigits, int scale) {
        return alnum(Pic.displaySigned(value, intDigits, scale), intDigits + scale);
    }

    public FieldEncoder binaryUnsigned(BigInteger value, int digits) {
        return twosComplement(value, FieldCursor.binaryLength(digits));
    }

    public FieldEncoder binarySigned(BigDecimal value, int intDigits, int scale) {
        BigInteger unscaled = value.setScale(scale, RoundingMode.DOWN).unscaledValue();
        return twosComplement(unscaled, FieldCursor.binaryLength(intDigits + scale));
    }

    public FieldEncoder packed(BigDecimal value, int intDigits, int scale, boolean signed) {
        int len = FieldCursor.packedLength(intDigits + scale);
        BigInteger unscaled = value.setScale(scale, RoundingMode.DOWN).unscaledValue();
        String digits = unscaled.abs().toString();
        int nibbles = len * 2 - 1;
        digits = digits.length() > nibbles ? digits.substring(digits.length() - nibbles)
                : "0".repeat(nibbles - digits.length()) + digits;
        char sign = !signed ? 'F' : unscaled.signum() < 0 ? 'D' : 'C';
        String hex = digits + sign;
        for (int i = 0; i < len; i++) {
            out.write(Integer.parseInt(hex.substring(i * 2, i * 2 + 2), 16));
        }
        return this;
    }

    public FieldEncoder fill(byte b, int len) {
        for (int i = 0; i < len; i++) {
            out.write(b);
        }
        return this;
    }

    public FieldEncoder spaces(int len) {
        return alnum("", len);
    }

    public FieldEncoder raw(byte[] bytes) {
        out.writeBytes(bytes);
        return this;
    }

    public int size() {
        return out.size();
    }

    public byte[] toBytes() {
        return out.toByteArray();
    }

    private FieldEncoder twosComplement(BigInteger value, int len) {
        byte[] full = value.toByteArray();
        byte pad = (byte) (value.signum() < 0 ? 0xFF : 0x00);
        for (int i = 0; i < len; i++) {
            int src = full.length - len + i;
            out.write(src < 0 ? pad : full[src]);
        }
        return this;
    }
}
