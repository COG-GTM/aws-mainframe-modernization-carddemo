package com.carddemo.programs.dbunldgs.copybook;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.nio.charset.Charset;
import java.util.Arrays;

/** Byte-level encoders/decoders for the COBOL USAGE types used by the copybooks. */
public final class CobolCodec {

    /** Mainframe code page used by the GSAM / IMS datasets (EBCDIC US/Canada). */
    public static final Charset EBCDIC = Charset.forName("IBM037");

    private CobolCodec() {
    }

    /** Number of bytes a PIC S9(digits) COMP-3 field occupies. */
    public static int packedLength(int digits) {
        return digits / 2 + 1;
    }

    /** IS NUMERIC test for a COMP-3 field: every digit nibble 0-9 and a valid sign nibble. */
    public static boolean isNumericPacked(byte[] buf, int off, int len) {
        for (int i = 0; i < len; i++) {
            int b = buf[off + i] & 0xFF;
            int hi = b >>> 4;
            int lo = b & 0x0F;
            if (hi > 9) {
                return false;
            }
            if (i < len - 1) {
                if (lo > 9) {
                    return false;
                }
            } else if (lo < 0x0A) {
                return false;
            }
        }
        return true;
    }

    /** Decode PIC S9(digits-scale)V9(scale) COMP-3. Throws {@link CobolDataException} on invalid data (S0C7). */
    public static BigDecimal decodePacked(byte[] buf, int off, int digits, int scale) {
        int len = packedLength(digits);
        if (!isNumericPacked(buf, off, len)) {
            throw new CobolDataException("Invalid packed decimal: " + hex(buf, off, len));
        }
        StringBuilder sb = new StringBuilder(len * 2);
        for (int i = 0; i < len; i++) {
            int b = buf[off + i] & 0xFF;
            sb.append((char) ('0' + (b >>> 4)));
            if (i < len - 1) {
                sb.append((char) ('0' + (b & 0x0F)));
            }
        }
        int sign = buf[off + len - 1] & 0x0F;
        BigInteger unscaled = new BigInteger(sb.toString());
        if (sign == 0x0B || sign == 0x0D) {
            unscaled = unscaled.negate();
        }
        return new BigDecimal(unscaled, scale);
    }

    /**
     * Encode PIC S9(digits-scale)V9(scale) COMP-3 with COBOL MOVE semantics: excess fraction digits
     * are truncated (not rounded) and high-order integer digits are dropped. Sign nibble C or D.
     */
    public static void encodePacked(BigDecimal value, byte[] buf, int off, int digits, int scale) {
        BigInteger unscaled = value.setScale(scale, RoundingMode.DOWN).unscaledValue();
        boolean negative = unscaled.signum() < 0;
        BigInteger magnitude = unscaled.abs().mod(BigInteger.TEN.pow(digits));
        int len = packedLength(digits);
        String s = magnitude.toString();
        int nibbles = len * 2 - 1;
        s = "0".repeat(nibbles - s.length()) + s;
        for (int i = 0; i < len; i++) {
            int hi = s.charAt(i * 2) - '0';
            int lo = (i == len - 1) ? (negative && magnitude.signum() != 0 ? 0x0D : 0x0C) : s.charAt(i * 2 + 1) - '0';
            buf[off + i] = (byte) ((hi << 4) | lo);
        }
    }

    /** Decode unsigned zoned decimal PIC 9(len) in the given code page. */
    public static long decodeZoned(byte[] buf, int off, int len, Charset cs) {
        String s = new String(buf, off, len, cs);
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) < '0' || s.charAt(i) > '9') {
                throw new CobolDataException("Invalid zoned decimal: '" + s + "'");
            }
        }
        return Long.parseLong(s);
    }

    /** Encode unsigned zoned decimal PIC 9(len): sign dropped, high-order digits truncated. */
    public static void encodeZoned(long value, byte[] buf, int off, int len, Charset cs) {
        String s = Long.toString(Math.abs(value));
        if (s.length() > len) {
            s = s.substring(s.length() - len);
        } else {
            s = "0".repeat(len - s.length()) + s;
        }
        System.arraycopy(s.getBytes(cs), 0, buf, off, len);
    }

    /** PIC X(len) as a Java string of exactly len characters. */
    public static String decodeAlpha(byte[] buf, int off, int len, Charset cs) {
        return new String(buf, off, len, cs);
    }

    /** PIC X(len) MOVE: left-justified, space padded, truncated on the right. */
    public static void encodeAlpha(String value, byte[] buf, int off, int len, Charset cs) {
        byte[] src = (value == null ? "" : value).getBytes(cs);
        byte space = " ".getBytes(cs)[0];
        Arrays.fill(buf, off, off + len, space);
        System.arraycopy(src, 0, buf, off, Math.min(len, src.length));
    }

    /** Fixed-length PIC X(len) value as Java would MOVE it: pad with spaces or truncate on the right. */
    public static String pic(String value, int len) {
        String v = value == null ? "" : value;
        return v.length() >= len ? v.substring(0, len) : v + " ".repeat(len - v.length());
    }

    /** PIC S9(4) COMP (halfword, big-endian). */
    public static short decodeHalfword(byte[] buf, int off) {
        return (short) (((buf[off] & 0xFF) << 8) | (buf[off + 1] & 0xFF));
    }

    public static void encodeHalfword(short value, byte[] buf, int off) {
        buf[off] = (byte) (value >>> 8);
        buf[off + 1] = (byte) value;
    }

    /** PIC S9(5)..S9(9) COMP (fullword, big-endian). */
    public static int decodeFullword(byte[] buf, int off) {
        return ((buf[off] & 0xFF) << 24) | ((buf[off + 1] & 0xFF) << 16) | ((buf[off + 2] & 0xFF) << 8) | (buf[off + 3] & 0xFF);
    }

    public static void encodeFullword(int value, byte[] buf, int off) {
        buf[off] = (byte) (value >>> 24);
        buf[off + 1] = (byte) (value >>> 16);
        buf[off + 2] = (byte) (value >>> 8);
        buf[off + 3] = (byte) value;
    }

    static void requireLength(byte[] buf, int expected, String what) {
        if (buf.length != expected) {
            throw new IllegalArgumentException(what + " must be " + expected + " bytes, got " + buf.length);
        }
    }

    public static String hex(byte[] buf, int off, int len) {
        StringBuilder sb = new StringBuilder(len * 2);
        for (int i = 0; i < len; i++) {
            sb.append(String.format("%02X", buf[off + i] & 0xFF));
        }
        return sb.toString();
    }
}
