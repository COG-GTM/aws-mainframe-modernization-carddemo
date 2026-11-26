package com.carddemo.programs.paudblod.copybook;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.nio.charset.Charset;
import java.util.Arrays;

/** Byte-level codecs for the COBOL USAGE clauses used by the PAUDBLOD copybooks. */
public final class CobolFields {

    private CobolFields() {
    }

    /** Bytes occupied by a COMP-3 item with {@code digits} total digits. */
    public static int packedLength(int digits) {
        return digits / 2 + 1;
    }

    /** {@code IS NUMERIC} for a signed COMP-3 item: digit nibbles 0-9, sign nibble C, D or F (NUMCLS(PRIM)). */
    public static boolean isPackedNumeric(byte[] buf, int off, int len) {
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
            } else if (lo != 0x0C && lo != 0x0D && lo != 0x0F) {
                return false;
            }
        }
        return true;
    }

    /** Decodes a COMP-3 field. Throws if the field is not numeric. */
    public static BigDecimal decodePacked(byte[] buf, int off, int len, int scale) {
        if (!isPackedNumeric(buf, off, len)) {
            throw new IllegalArgumentException("Invalid packed decimal: " + hex(buf, off, len));
        }
        StringBuilder sb = new StringBuilder(len * 2);
        for (int i = 0; i < len; i++) {
            int b = buf[off + i] & 0xFF;
            sb.append((char) ('0' + (b >>> 4)));
            if (i < len - 1) {
                sb.append((char) ('0' + (b & 0x0F)));
            }
        }
        BigInteger unscaled = new BigInteger(sb.toString());
        if ((buf[off + len - 1] & 0x0F) == 0x0D) {
            unscaled = unscaled.negate();
        }
        return new BigDecimal(unscaled, scale);
    }

    /**
     * Encodes a value into a signed COMP-3 field of {@code digits} digits and {@code scale} decimals,
     * using COBOL MOVE semantics: excess decimals are truncated and high-order digits are dropped.
     */
    public static void encodePacked(BigDecimal value, int digits, int scale, byte[] buf, int off) {
        BigInteger unscaled = value.setScale(scale, RoundingMode.DOWN).unscaledValue();
        boolean negative = unscaled.signum() < 0;
        String d = unscaled.abs().toString();
        if (d.length() > digits) {
            d = d.substring(d.length() - digits);
        }
        int len = packedLength(digits);
        String padded = "0".repeat(len * 2 - 1 - d.length()) + d;
        if (negative && padded.chars().allMatch(c -> c == '0')) {
            negative = false;
        }
        for (int i = 0; i < len; i++) {
            int hi = padded.charAt(i * 2) - '0';
            int lo = i == len - 1 ? (negative ? 0x0D : 0x0C) : padded.charAt(i * 2 + 1) - '0';
            buf[off + i] = (byte) ((hi << 4) | lo);
        }
    }

    /** Decodes a signed big-endian binary (COMP) field of 2 or 4 bytes. */
    public static int decodeBinary(byte[] buf, int off, int len) {
        return switch (len) {
            case 2 -> (short) (((buf[off] & 0xFF) << 8) | (buf[off + 1] & 0xFF));
            case 4 -> ((buf[off] & 0xFF) << 24) | ((buf[off + 1] & 0xFF) << 16)
                    | ((buf[off + 2] & 0xFF) << 8) | (buf[off + 3] & 0xFF);
            default -> throw new IllegalArgumentException("Unsupported binary length " + len);
        };
    }

    /** Encodes a signed big-endian binary (COMP) field, truncating to the PIC digit count. */
    public static void encodeBinary(long value, int digits, int len, byte[] buf, int off) {
        long mod = BigInteger.TEN.pow(digits).longValueExact();
        long v = value % mod;
        for (int i = len - 1; i >= 0; i--) {
            buf[off + i] = (byte) (v & 0xFF);
            v >>= 8;
        }
    }

    /** Decodes an unsigned zoned-decimal (DISPLAY) PIC 9(n) field. */
    public static long decodeZoned(byte[] buf, int off, int len, Charset cs) {
        String s = decodeText(buf, off, len, cs);
        long v = 0;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c < '0' || c > '9') {
                throw new IllegalArgumentException("Invalid zoned decimal: '" + s + "'");
            }
            v = v * 10 + (c - '0');
        }
        return v;
    }

    /** Encodes an unsigned PIC 9(n) DISPLAY field (absolute value, high-order truncation). */
    public static void encodeZoned(long value, int len, byte[] buf, int off, Charset cs) {
        String d = Long.toString(Math.abs(value));
        if (d.length() > len) {
            d = d.substring(d.length() - len);
        }
        encodeText("0".repeat(len - d.length()) + d, len, buf, off, cs);
    }

    /** Decodes a PIC X(n) field, keeping trailing spaces (fixed length). */
    public static String decodeText(byte[] buf, int off, int len, Charset cs) {
        return new String(buf, off, len, cs);
    }

    /** Encodes a PIC X(n) field: left-justified, space padded, right-truncated. */
    public static void encodeText(String value, int len, byte[] buf, int off, Charset cs) {
        byte[] space = " ".getBytes(cs);
        Arrays.fill(buf, off, off + len, space[0]);
        byte[] src = (value == null ? "" : value).getBytes(cs);
        System.arraycopy(src, 0, buf, off, Math.min(len, src.length));
    }

    /** Pads/truncates a Java string to a PIC X(n) value. */
    public static String fixed(String value, int len) {
        String v = value == null ? "" : value;
        return v.length() >= len ? v.substring(0, len) : v + " ".repeat(len - v.length());
    }

    public static String hex(byte[] buf, int off, int len) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < len; i++) {
            sb.append(String.format("%02X", buf[off + i] & 0xFF));
        }
        return sb.toString();
    }
}
