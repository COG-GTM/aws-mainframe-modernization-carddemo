package com.carddemo.programs.paudbunl.support;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.nio.charset.Charset;
import java.util.Locale;
import java.util.Arrays;

/** Byte-level codecs for the COBOL USAGE clauses used by the PAUDBUNL copybooks. */
public final class CobolData {

    /** Mainframe code page used by the IMS segments and the unload files. */
    public static final Charset EBCDIC = Charset.forName("IBM037");

    private static final BigInteger TEN = BigInteger.TEN;

    private CobolData() {
    }

    /** Bytes occupied by a COMP-3 field with the given number of digits. */
    public static int packedLength(int digits) {
        return digits / 2 + 1;
    }

    /**
     * IBM {@code IS NUMERIC} for a signed COMP-3 field: every digit nibble 0-9 and a
     * sign nibble of C, D or F (F accepted, as under NUMPROC(NOPFD)/MIG).
     */
    public static boolean isNumericPacked(byte[] buf, int off, int len) {
        for (int i = 0; i < len; i++) {
            int hi = (buf[off + i] >> 4) & 0x0F;
            int lo = buf[off + i] & 0x0F;
            if (hi > 9) {
                return false;
            }
            if (i < len - 1) {
                if (lo > 9) {
                    return false;
                }
            } else if (lo != 0x0C && lo != 0x0D && lo != 0x0F) {
                return false;
            }
        }
        return true;
    }

    /** Decodes a signed COMP-3 field; throws {@link IllegalArgumentException} on invalid data (S0C7). */
    public static BigDecimal decodePacked(byte[] buf, int off, int len, int scale) {
        if (!isNumericPacked(buf, off, len)) {
            throw new IllegalArgumentException("Invalid packed decimal: " + hex(buf, off, len));
        }
        BigInteger value = BigInteger.ZERO;
        for (int i = 0; i < len; i++) {
            int hi = (buf[off + i] >> 4) & 0x0F;
            value = value.multiply(TEN).add(BigInteger.valueOf(hi));
            if (i < len - 1) {
                value = value.multiply(TEN).add(BigInteger.valueOf(buf[off + i] & 0x0F));
            }
        }
        if ((buf[off + len - 1] & 0x0F) == 0x0D) {
            value = value.negate();
        }
        return new BigDecimal(value, scale);
    }

    /**
     * Encodes a value into a signed COMP-3 field of {@code digits} digits and {@code scale}
     * decimals. Like a COBOL MOVE: excess decimals are truncated and high-order digits dropped.
     */
    public static void encodePacked(BigDecimal value, int digits, int scale, byte[] buf, int off) {
        BigInteger unscaled = value.setScale(scale, RoundingMode.DOWN).unscaledValue();
        boolean negative = unscaled.signum() < 0;
        unscaled = unscaled.abs().mod(TEN.pow(digits));
        int len = packedLength(digits);
        String text = unscaled.toString();
        char[] nibbles = new char[len * 2 - 1];
        Arrays.fill(nibbles, '0');
        text.getChars(0, text.length(), nibbles, nibbles.length - text.length());
        for (int i = 0; i < len; i++) {
            int hi = nibbles[i * 2] - '0';
            int lo = i < len - 1 ? nibbles[i * 2 + 1] - '0' : (negative ? 0x0D : 0x0C);
            buf[off + i] = (byte) ((hi << 4) | lo);
        }
    }

    /** Decodes a big-endian signed binary (COMP) field of 2, 4 or 8 bytes. */
    public static long decodeBinary(byte[] buf, int off, int len) {
        long v = buf[off];
        for (int i = 1; i < len; i++) {
            v = (v << 8) | (buf[off + i] & 0xFF);
        }
        return v;
    }

    /** Encodes a big-endian signed binary (COMP) field of 2, 4 or 8 bytes. */
    public static void encodeBinary(long value, byte[] buf, int off, int len) {
        for (int i = len - 1; i >= 0; i--) {
            buf[off + i] = (byte) value;
            value >>= 8;
        }
    }

    /** Decodes a PIC X(n) field. */
    public static String decodeAlpha(byte[] buf, int off, int len, Charset cs) {
        return new String(buf, off, len, cs);
    }

    /** Encodes a PIC X(n) field: left-justified, space-padded, right-truncated (COBOL MOVE). */
    public static void encodeAlpha(String value, byte[] buf, int off, int len, Charset cs) {
        String v = value == null ? "" : value;
        byte[] spaces = " ".repeat(len).getBytes(cs);
        System.arraycopy(spaces, 0, buf, off, len);
        byte[] b = v.getBytes(cs);
        System.arraycopy(b, 0, buf, off, Math.min(len, b.length));
    }

    /** Decodes an unsigned zoned PIC 9(n) DISPLAY field. */
    public static long decodeZoned(byte[] buf, int off, int len, Charset cs) {
        String s = decodeAlpha(buf, off, len, cs);
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) < '0' || s.charAt(i) > '9') {
                throw new IllegalArgumentException("Invalid zoned decimal: '" + s + "'");
            }
        }
        return Long.parseLong(s);
    }

    /** Encodes an unsigned zoned PIC 9(n) DISPLAY field; high-order digits are truncated. */
    public static void encodeZoned(long value, byte[] buf, int off, int len, Charset cs) {
        long v = len >= 19 ? Math.abs(value) : Math.abs(value) % BigInteger.TEN.pow(len).longValueExact();
        String s = String.format(Locale.ROOT, "%0" + len + "d", v);
        encodeAlpha(s, buf, off, len, cs);
    }

    /**
     * {@code ADD 1 TO counter} for an unsigned/positive counter of {@code digits} digits
     * without ON SIZE ERROR: the high-order digit is lost on overflow.
     */
    public static long addTruncating(long counter, long increment, int digits) {
        long modulus = BigInteger.TEN.pow(digits).longValueExact();
        return Math.floorMod(counter + increment, modulus);
    }

    public static String hex(byte[] buf, int off, int len) {
        StringBuilder sb = new StringBuilder(len * 2);
        for (int i = 0; i < len; i++) {
            sb.append(String.format(Locale.ROOT, "%02X", buf[off + i] & 0xFF));
        }
        return sb.toString();
    }
}
