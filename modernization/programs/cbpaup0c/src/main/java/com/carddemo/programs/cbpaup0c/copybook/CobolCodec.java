package com.carddemo.programs.cbpaup0c.copybook;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.Charset;
import java.util.Arrays;

/** Byte-level codecs for the USAGE DISPLAY / COMP / COMP-3 fields used by the segment copybooks. */
public final class CobolCodec {

    /** Host code page of the CardDemo IMS unload files. */
    public static final Charset EBCDIC = Charset.forName("IBM037");

    private CobolCodec() {
    }

    /**
     * Decodes a packed-decimal (COMP-3) field. Digit nibbles must be 0-9; the sign nibble is read the
     * way an IBM UNPK/ZAP move would treat it: X'B' and X'D' are negative, anything else positive.
     */
    public static BigInteger decodePackedUnscaled(byte[] buf, int offset, int length) {
        StringBuilder digits = new StringBuilder(length * 2);
        for (int i = 0; i < length; i++) {
            int b = buf[offset + i] & 0xFF;
            int hi = b >>> 4;
            int lo = b & 0x0F;
            digits.append(digit(hi, offset + i));
            if (i < length - 1) {
                digits.append(digit(lo, offset + i));
            } else {
                BigInteger value = new BigInteger(digits.toString());
                return (lo == 0x0B || lo == 0x0D) ? value.negate() : value;
            }
        }
        throw new IllegalArgumentException("empty packed field");
    }

    private static char digit(int nibble, int pos) {
        if (nibble > 9) {
            throw new IllegalArgumentException(
                    "Data exception (S0C7): invalid packed digit nibble at offset " + pos);
        }
        return (char) ('0' + nibble);
    }

    public static long decodePackedLong(byte[] buf, int offset, int length) {
        return decodePackedUnscaled(buf, offset, length).longValueExact();
    }

    public static BigDecimal decodePacked(byte[] buf, int offset, int length, int scale) {
        return new BigDecimal(decodePackedUnscaled(buf, offset, length), scale);
    }

    /** Encodes a packed-decimal field with preferred signs C/D, truncating high-order digits like a COBOL MOVE. */
    public static void encodePacked(BigInteger unscaled, byte[] buf, int offset, int length) {
        int digitCount = length * 2 - 1;
        String digits = unscaled.abs().toString();
        if (digits.length() > digitCount) {
            digits = digits.substring(digits.length() - digitCount);
        }
        digits = "0".repeat(digitCount - digits.length()) + digits;
        int sign = unscaled.signum() < 0 ? 0x0D : 0x0C;
        for (int i = 0; i < length; i++) {
            int hi = digits.charAt(i * 2) - '0';
            int lo = (i == length - 1) ? sign : digits.charAt(i * 2 + 1) - '0';
            buf[offset + i] = (byte) ((hi << 4) | lo);
        }
    }

    public static void encodePacked(long value, byte[] buf, int offset, int length) {
        encodePacked(BigInteger.valueOf(value), buf, offset, length);
    }

    public static void encodePacked(BigDecimal value, int scale, byte[] buf, int offset, int length) {
        encodePacked(value.setScale(scale, java.math.RoundingMode.DOWN).unscaledValue(), buf, offset, length);
    }

    /** Big-endian two's-complement halfword (PIC S9(4) COMP). */
    public static int decodeHalfword(byte[] buf, int offset) {
        return (short) (((buf[offset] & 0xFF) << 8) | (buf[offset + 1] & 0xFF));
    }

    public static void encodeHalfword(int value, byte[] buf, int offset) {
        buf[offset] = (byte) (value >>> 8);
        buf[offset + 1] = (byte) value;
    }

    public static String decodeText(byte[] buf, int offset, int length, Charset charset) {
        return new String(buf, offset, length, charset);
    }

    /** MOVE to PIC X(n): left-justified, space-padded, truncated on the right. */
    public static void encodeText(String value, byte[] buf, int offset, int length, Charset charset) {
        byte[] spaces = " ".getBytes(charset);
        Arrays.fill(buf, offset, offset + length, spaces[0]);
        byte[] bytes = (value == null ? "" : value).getBytes(charset);
        System.arraycopy(bytes, 0, buf, offset, Math.min(bytes.length, length));
    }

    /** Pads or truncates to the exact PIC X(n) length. */
    public static String fit(String value, int length) {
        String v = value == null ? "" : value;
        return v.length() >= length ? v.substring(0, length) : v + " ".repeat(length - v.length());
    }
}
