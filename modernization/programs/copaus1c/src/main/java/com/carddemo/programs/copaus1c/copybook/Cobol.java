package com.carddemo.programs.copaus1c.copybook;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.Charset;
import java.util.Arrays;

/** COBOL storage helpers: fixed-width alphanumerics, zoned/packed decimal, EBCDIC (CP037). */
public final class Cobol {

    public static final Charset EBCDIC = Charset.forName("IBM037");
    public static final char LOW_VALUE = '\u0000';

    private Cobol() {
    }

    /** MOVE alphanumeric to PIC X(len): left-justify, pad with spaces, truncate on the right. */
    public static String x(String value, int len) {
        String v = value == null ? "" : value;
        if (v.length() >= len) {
            return v.substring(0, len);
        }
        return v + " ".repeat(len - v.length());
    }

    public static String spaces(int len) {
        return " ".repeat(len);
    }

    public static String lowValues(int len) {
        return String.valueOf(LOW_VALUE).repeat(len);
    }

    public static boolean isSpaces(String s) {
        return s.chars().allMatch(c -> c == ' ');
    }

    public static boolean isLowValues(String s) {
        return s.chars().allMatch(c -> c == LOW_VALUE);
    }

    /** IS NUMERIC class test for an unsigned display field. */
    public static boolean isNumeric(String s) {
        return !s.isEmpty() && s.chars().allMatch(c -> c >= '0' && c <= '9');
    }

    /** MOVE unsigned integer to PIC 9(len) DISPLAY: keep the low-order digits, zero-fill. */
    public static String nine(long value, int len) {
        String digits = Long.toString(Math.abs(value));
        if (digits.length() > len) {
            digits = digits.substring(digits.length() - len);
        }
        return "0".repeat(len - digits.length()) + digits;
    }

    /**
     * Numeric value of a PIC 9(n) DISPLAY field as the compiler converts it (digit nibble of each byte),
     * so non-numeric content yields the same value the mainframe would compute instead of failing.
     */
    public static long displayValue(String s) {
        long v = 0;
        for (int i = 0; i < s.length(); i++) {
            v = v * 10 + Math.min(s.charAt(i) & 0x0F, 9);
        }
        return v;
    }

    /** Overlay {@code src} at 1-based position {@code pos} of {@code target} (reference modification MOVE). */
    public static String overlay(String target, int pos, String src) {
        StringBuilder sb = new StringBuilder(target);
        int start = pos - 1;
        for (int i = 0; i < src.length() && start + i < sb.length(); i++) {
            sb.setCharAt(start + i, src.charAt(i));
        }
        return sb.toString();
    }

    // ---------------------------------------------------------------- EBCDIC record codecs

    public static String readX(byte[] rec, int off, int len) {
        return new String(rec, off, len, EBCDIC);
    }

    public static void writeX(byte[] rec, int off, int len, String value) {
        byte[] b = x(value, len).getBytes(EBCDIC);
        System.arraycopy(b, 0, rec, off, len);
    }

    /** Packed decimal (COMP-3): {@code len} bytes, sign in the low nibble of the last byte (D/B = negative). */
    public static BigDecimal readPacked(byte[] rec, int off, int len, int scale) {
        StringBuilder digits = new StringBuilder();
        for (int i = 0; i < len; i++) {
            int b = rec[off + i] & 0xFF;
            digits.append(Math.min(b >> 4, 9));
            if (i < len - 1) {
                digits.append(Math.min(b & 0x0F, 9));
            }
        }
        int sign = rec[off + len - 1] & 0x0F;
        BigInteger unscaled = new BigInteger(digits.toString());
        if (sign == 0x0D || sign == 0x0B) {
            unscaled = unscaled.negate();
        }
        return new BigDecimal(unscaled, scale);
    }

    /** True when every digit nibble is 0-9 and the sign nibble is A-F (a valid COMP-3 value). */
    public static boolean isValidPacked(byte[] rec, int off, int len) {
        for (int i = 0; i < len; i++) {
            int b = rec[off + i] & 0xFF;
            if ((b >> 4) > 9 || (i < len - 1 && (b & 0x0F) > 9) || (i == len - 1 && (b & 0x0F) < 0x0A)) {
                return false;
            }
        }
        return true;
    }

    /** Write packed decimal; high-order digits beyond the field size are truncated as in a COBOL MOVE. */
    public static void writePacked(byte[] rec, int off, int len, int scale, BigDecimal value) {
        writePacked(rec, off, len, scale, value, len * 2 - 1);
    }

    /** Write packed decimal for a PIC with {@code digits} digits (an even count leaves the pad nibble zero). */
    public static void writePacked(byte[] rec, int off, int len, int scale, BigDecimal value, int digits) {
        BigInteger unscaled = value.setScale(scale, java.math.RoundingMode.DOWN).unscaledValue();
        boolean negative = unscaled.signum() < 0;
        int ndigits = len * 2 - 1;
        String d = unscaled.abs().toString();
        if (d.length() > digits) {
            d = d.substring(d.length() - digits);
        }
        d = "0".repeat(ndigits - d.length()) + d + (negative ? "D" : "C");
        for (int i = 0; i < len; i++) {
            rec[off + i] = (byte) Integer.parseInt(d.substring(i * 2, i * 2 + 2), 16);
        }
    }

    /** Binary halfword/fullword (COMP), big-endian, signed. */
    public static int readBinary(byte[] rec, int off, int len) {
        int v = rec[off];
        for (int i = 1; i < len; i++) {
            v = (v << 8) | (rec[off + i] & 0xFF);
        }
        return v;
    }

    public static void writeBinary(byte[] rec, int off, int len, int value) {
        for (int i = len - 1; i >= 0; i--) {
            rec[off + i] = (byte) value;
            value >>= 8;
        }
    }

    public static byte[] copy(byte[] b) {
        return Arrays.copyOf(b, b.length);
    }
}
