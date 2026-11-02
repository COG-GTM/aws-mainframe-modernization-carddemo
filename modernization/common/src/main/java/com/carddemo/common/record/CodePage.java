package com.carddemo.common.record;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;

/**
 * Character set of a record's {@code DISPLAY} bytes. Packed and binary fields are the same bytes in
 * either code page; only {@code PIC X} and zoned decimal fields differ.
 */
public enum CodePage {

    /**
     * The {@code app/data/ASCII} extracts. Decoded as ISO-8859-1 so every byte maps to one char and
     * back. Signed zoned decimals carry the sign as an overpunch character on the last digit:
     * {@code '{'}, {@code 'A'}-{@code 'I'} = +0..+9 and {@code '}'}, {@code 'J'}-{@code 'R'} = -0..-9.
     */
    ASCII(StandardCharsets.ISO_8859_1) {
        @Override
        int lastDigit(byte b, NumericField field) {
            int c = b & 0xFF;
            if (c >= '0' && c <= '9') {
                return c - '0';
            }
            if (c == '{') {
                return 0;
            }
            if (c >= 'A' && c <= 'I') {
                return c - 'A' + 1;
            }
            if (c == '}') {
                return -10;
            }
            if (c >= 'J' && c <= 'R') {
                return -(c - 'J' + 1) - 10;
            }
            // Micro Focus / GnuCOBOL ASCII sign convention: negative digits 0x70-0x79 ('p'-'y').
            if (c >= 0x70 && c <= 0x79) {
                return -(c - 0x70) - 10;
            }
            throw new IllegalArgumentException("invalid zoned sign byte 0x" + Integer.toHexString(c));
        }

        @Override
        boolean isDigit(byte b) {
            return b >= '0' && b <= '9';
        }

        @Override
        byte digit(int value) {
            return (byte) ('0' + value);
        }

        @Override
        byte signedLast(int value, boolean negative) {
            if (negative) {
                return (byte) (value == 0 ? '}' : 'J' + value - 1);
            }
            return (byte) (value == 0 ? '{' : 'A' + value - 1);
        }
    },

    /**
     * IBM code page 037 (US/Canada EBCDIC), the {@code app/data/EBCDIC/*.PS} files. Zoned digits are
     * {@code 0xF0}-{@code 0xF9}; the zone nibble of the last byte is the sign ({@code C}/{@code A}/{@code E}
     * or {@code F} positive, {@code D}/{@code B} negative).
     */
    EBCDIC(Charset.forName("IBM037")) {
        @Override
        int lastDigit(byte b, NumericField field) {
            int zone = (b >> 4) & 0x0F;
            int digit = b & 0x0F;
            if (digit > 9) {
                throw new IllegalArgumentException("invalid zoned digit nibble in 0x" + Integer.toHexString(b & 0xFF));
            }
            return switch (zone) {
                case 0xF, 0xC, 0xA, 0xE -> digit;
                case 0xD, 0xB -> -digit - 10;
                default -> throw new IllegalArgumentException("invalid zone nibble in 0x" + Integer.toHexString(b & 0xFF));
            };
        }

        @Override
        boolean isDigit(byte b) {
            return (b & 0xF0) == 0xF0 && (b & 0x0F) <= 9;
        }

        @Override
        byte digit(int value) {
            return (byte) (0xF0 | value);
        }

        @Override
        byte signedLast(int value, boolean negative) {
            return (byte) ((negative ? 0xD0 : 0xC0) | value);
        }
    };

    private final Charset charset;

    CodePage(Charset charset) {
        this.charset = charset;
    }

    public Charset charset() {
        return charset;
    }

    /** The space character in this code page (pads {@code PIC X} fields and short line-sequential records). */
    public byte space() {
        return encodeText(" ")[0];
    }

    public String decodeText(byte[] data, int offset, int length) {
        return new String(data, offset, length, charset);
    }

    /** Encodes text one byte per character; characters the code page cannot represent are rejected. */
    public byte[] encodeText(String text) {
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

    BigDecimal decodeZoned(byte[] data, int offset, NumericField field) {
        int length = field.length();
        StringBuilder digits = new StringBuilder(length);
        for (int i = 0; i < length - 1; i++) {
            byte b = data[offset + i];
            if (!isDigit(b)) {
                throw new IllegalArgumentException("non-numeric byte 0x" + Integer.toHexString(b & 0xFF)
                        + " at position " + i);
            }
            digits.append((char) ('0' + (b & 0x0F)));
        }
        int last = lastDigit(data[offset + length - 1], field);
        boolean negative = last < 0;
        digits.append((char) ('0' + (negative ? -(last + 10) : last)));
        if (negative && !field.signed()) {
            throw new IllegalArgumentException("negative sign in unsigned field");
        }
        BigDecimal value = new BigDecimal(new BigInteger(digits.toString()), field.scale());
        return negative ? value.negate() : value;
    }

    /** Writes an already range-checked value; signed fields always carry the sign, unsigned ones none. */
    void encodeZoned(BigDecimal value, NumericField field, byte[] data, int offset) {
        String digits = value.unscaledValue().abs().toString();
        int length = field.length();
        int pad = length - digits.length();
        for (int i = 0; i < length; i++) {
            int d = i < pad ? 0 : digits.charAt(i - pad) - '0';
            data[offset + i] = i == length - 1 && field.signed() ? signedLast(d, value.signum() < 0) : digit(d);
        }
    }

    /** Last byte of a zoned field: digit 0..9 when positive, {@code -(digit) - 10} when negative. */
    abstract int lastDigit(byte b, NumericField field);

    abstract boolean isDigit(byte b);

    abstract byte digit(int value);

    abstract byte signedLast(int value, boolean negative);
}
