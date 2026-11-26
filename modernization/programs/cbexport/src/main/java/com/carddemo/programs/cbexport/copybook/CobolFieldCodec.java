package com.carddemo.programs.cbexport.copybook;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

/**
 * Byte-level encoders/decoders for the COBOL USAGE forms used by CBEXPORT's copybooks.
 * Character data is single-byte ISO-8859-1 (the ASCII rendering of the EBCDIC datasets);
 * signed zoned decimals use the trailing overpunch convention found in app/data/ASCII
 * ('{', 'A'-'I' positive; '}', 'J'-'R' negative).
 */
public final class CobolFieldCodec {

    public static final Charset CHARSET = StandardCharsets.ISO_8859_1;
    public static final byte SPACE = (byte) ' ';

    private CobolFieldCodec() {
    }

    // ---- PIC X(n) -------------------------------------------------------------------------

    public static String getAlnum(byte[] rec, int offset, int length) {
        return new String(rec, offset, length, CHARSET);
    }

    /** MOVE alphanumeric: left-justify, space-fill on the right, truncate on the right. */
    public static void putAlnum(byte[] rec, int offset, int length, String value) {
        Arrays.fill(rec, offset, offset + length, SPACE);
        if (value == null) {
            return;
        }
        byte[] src = value.getBytes(CHARSET);
        System.arraycopy(src, 0, rec, offset, Math.min(length, src.length));
    }

    /** Pads/truncates a Java string to an exact PIC X(n) image. */
    public static String fit(String value, int length) {
        byte[] b = new byte[length];
        putAlnum(b, 0, length, value);
        return getAlnum(b, 0, length);
    }

    // ---- PIC 9(n) / S9(n)V9(m) USAGE DISPLAY ------------------------------------------------

    /** Unsigned zoned decimal. Non-digit bytes contribute their low nibble, as the IBM MOVE does. */
    public static long getUnsignedZoned(byte[] rec, int offset, int length) {
        long v = 0;
        for (int i = 0; i < length; i++) {
            v = v * 10 + zoneDigit(rec[offset + i]);
        }
        return v;
    }

    public static BigDecimal getSignedZoned(byte[] rec, int offset, int length, int scale) {
        long v = 0;
        for (int i = 0; i < length - 1; i++) {
            v = v * 10 + zoneDigit(rec[offset + i]);
        }
        byte last = rec[offset + length - 1];
        v = v * 10 + overpunchDigit(last);
        if (overpunchNegative(last)) {
            v = -v;
        }
        return BigDecimal.valueOf(v, scale);
    }

    /** MOVE to PIC 9(n): sign dropped, high-order digits truncated, zero-filled on the left. */
    public static void putUnsignedZoned(byte[] rec, int offset, int length, long value) {
        String digits = truncatedDigits(BigInteger.valueOf(value).abs(), length);
        System.arraycopy(digits.getBytes(CHARSET), 0, rec, offset, length);
    }

    /** MOVE to PIC S9(n)V9(m) DISPLAY: decimal-aligned, truncated both ends, trailing overpunch sign. */
    public static void putSignedZoned(byte[] rec, int offset, int length, int scale, BigDecimal value) {
        BigInteger unscaled = value.setScale(scale, RoundingMode.DOWN).unscaledValue();
        boolean negative = unscaled.signum() < 0;
        String digits = truncatedDigits(unscaled.abs(), length);
        byte[] b = digits.getBytes(CHARSET);
        int lastDigit = b[length - 1] - '0';
        b[length - 1] = (byte) (negative
                ? (lastDigit == 0 ? '}' : 'J' + lastDigit - 1)
                : (lastDigit == 0 ? '{' : 'A' + lastDigit - 1));
        System.arraycopy(b, 0, rec, offset, length);
    }

    // ---- COMP-3 -------------------------------------------------------------------------------

    public static int packedLength(int digits) {
        return digits / 2 + 1;
    }

    /** MOVE to packed decimal. Sign nibble C/D when signed, F when the PIC is unsigned. */
    public static void putPacked(byte[] rec, int offset, int digits, int scale, boolean signed, BigDecimal value) {
        BigInteger unscaled = value.setScale(scale, RoundingMode.DOWN).unscaledValue();
        boolean negative = signed && unscaled.signum() < 0;
        int nibbles = packedLength(digits) * 2;
        String d = truncatedDigits(unscaled.abs(), digits);
        StringBuilder all = new StringBuilder();
        for (int i = 0; i < nibbles - 1 - digits; i++) {
            all.append('0');
        }
        all.append(d);
        int sign = signed ? (negative ? 0xD : 0xC) : 0xF;
        for (int i = 0; i < nibbles / 2; i++) {
            int hi = all.charAt(2 * i) - '0';
            int lo = (2 * i + 1 < all.length()) ? all.charAt(2 * i + 1) - '0' : sign;
            rec[offset + i] = (byte) ((hi << 4) | lo);
        }
    }

    public static BigDecimal getPacked(byte[] rec, int offset, int digits, int scale) {
        int len = packedLength(digits);
        long v = 0;
        for (int i = 0; i < len; i++) {
            int b = rec[offset + i] & 0xFF;
            v = v * 10 + (b >> 4);
            if (i < len - 1) {
                v = v * 10 + (b & 0x0F);
            } else if ((b & 0x0F) == 0xD || (b & 0x0F) == 0xB) {
                v = -v;
            }
        }
        return BigDecimal.valueOf(v, scale);
    }

    // ---- COMP (binary, big-endian, TRUNC(STD)) -----------------------------------------------

    public static int binaryLength(int digits) {
        if (digits <= 4) {
            return 2;
        }
        return digits <= 9 ? 4 : 8;
    }

    /** MOVE to binary: value truncated to the PIC digit count (TRUNC(STD)), stored big-endian. */
    public static void putBinary(byte[] rec, int offset, int digits, boolean signed, long value) {
        BigInteger mod = BigInteger.TEN.pow(digits);
        BigInteger v = BigInteger.valueOf(value);
        BigInteger mag = v.abs().mod(mod);
        long stored = (signed && v.signum() < 0) ? mag.negate().longValue() : mag.longValue();
        int len = binaryLength(digits);
        for (int i = len - 1; i >= 0; i--) {
            rec[offset + i] = (byte) stored;
            stored >>= 8;
        }
    }

    public static long getBinary(byte[] rec, int offset, int digits, boolean signed) {
        int len = binaryLength(digits);
        long v = signed && rec[offset] < 0 ? -1L : 0L;
        for (int i = 0; i < len; i++) {
            v = (v << 8) | (rec[offset + i] & 0xFF);
        }
        return v;
    }

    // ---- helpers -------------------------------------------------------------------------------

    private static String truncatedDigits(BigInteger nonNegative, int length) {
        String s = nonNegative.toString();
        if (s.length() > length) {
            return s.substring(s.length() - length);
        }
        return "0".repeat(length - s.length()) + s;
    }

    private static int zoneDigit(byte b) {
        int c = b & 0xFF;
        if (c >= '0' && c <= '9') {
            return c - '0';
        }
        return (c & 0x0F) % 10;
    }

    private static int overpunchDigit(byte b) {
        int c = b & 0xFF;
        if (c == '{' || c == '}') {
            return 0;
        }
        if (c >= 'A' && c <= 'I') {
            return c - 'A' + 1;
        }
        if (c >= 'J' && c <= 'R') {
            return c - 'J' + 1;
        }
        return zoneDigit(b);
    }

    private static boolean overpunchNegative(byte b) {
        int c = b & 0xFF;
        return c == '}' || (c >= 'J' && c <= 'R');
    }
}
