package com.carddemo.batch.codec;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * USAGE COMP-3 (packed decimal): two digits per byte, sign in the low nibble of the last byte
 * ({@code C} positive, {@code D} negative, {@code F} unsigned accepted on read). A PIC S9(n) item
 * occupies {@code n / 2 + 1} bytes, so PIC S9(10)V99 is 7 bytes / 13 digit nibbles.
 */
public final class PackedDecimal {

    private PackedDecimal() {
    }

    public static int bytesFor(int digits) {
        return digits / 2 + 1;
    }

    public static BigDecimal decode(byte[] buf, int off, int len, int scale) {
        StringBuilder digits = new StringBuilder(len * 2);
        for (int i = 0; i < len; i++) {
            int b = buf[off + i] & 0xFF;
            int hi = b >> 4;
            int lo = b & 0x0F;
            if (hi > 9) {
                throw new CodecException("bad COMP-3 digit nibble " + hi + " at offset " + (off + i));
            }
            digits.append((char) ('0' + hi));
            if (i < len - 1) {
                if (lo > 9) {
                    throw new CodecException("bad COMP-3 digit nibble " + lo + " at offset " + (off + i));
                }
                digits.append((char) ('0' + lo));
            } else if (lo != 0x0C && lo != 0x0D && lo != 0x0F) {
                throw new CodecException("bad COMP-3 sign nibble " + Integer.toHexString(lo) + " at offset " + (off + i));
            } else if (lo == 0x0D) {
                return new BigDecimal(new BigInteger(digits.toString()), scale).negate();
            }
        }
        return new BigDecimal(new BigInteger(digits.toString()), scale);
    }

    public static void encode(byte[] buf, int off, int len, int scale, boolean signed, BigDecimal value) {
        BigDecimal scaled = value.setScale(scale, RoundingMode.DOWN);
        boolean negative = scaled.signum() < 0;
        int ndig = len * 2 - 1;
        String digits = scaled.abs().unscaledValue().toString();
        if (digits.length() > ndig) {
            digits = digits.substring(digits.length() - ndig);
        }
        StringBuilder sb = new StringBuilder(ndig + 1);
        for (int i = digits.length(); i < ndig; i++) {
            sb.append('0');
        }
        sb.append(digits);
        int sign = negative ? 0x0D : (signed ? 0x0C : 0x0F);
        for (int i = 0; i < len; i++) {
            int hi = sb.charAt(2 * i) - '0';
            int lo = (i == len - 1) ? sign : sb.charAt(2 * i + 1) - '0';
            buf[off + i] = (byte) ((hi << 4) | lo);
        }
    }

    /** COBOL {@code INITIALIZE} of a signed COMP-3 item: all zero digits with a positive {@code C} sign. */
    public static void initialize(byte[] buf, int off, int len) {
        for (int i = 0; i < len - 1; i++) {
            buf[off + i] = 0;
        }
        buf[off + len - 1] = 0x0C;
    }
}
