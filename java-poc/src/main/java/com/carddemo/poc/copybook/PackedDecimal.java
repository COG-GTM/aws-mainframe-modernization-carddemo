package com.carddemo.poc.copybook;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * Encoding/decoding of COBOL packed-decimal ({@code USAGE COMP-3}) fields.
 *
 * <p>Two digits per byte, the low nibble of the last byte holds the sign
 * ({@code 0xC} positive, {@code 0xD} negative, {@code 0xF} unsigned).
 * {@code PIC S9(n)V99 COMP-3} occupies {@code (n + 2) / 2 + 1} bytes.
 *
 * <p>Neither CBACT03C nor the input side of CBACT01C reads COMP-3 data, but CBACT01C
 * <em>writes</em> {@code OUT-ACCT-CURR-CYC-DEBIT} and {@code ARR-ACCT-CURR-CYC-DEBIT} as
 * COMP-3, so the codec is included to demonstrate the mapping.
 */
public final class PackedDecimal {

    private PackedDecimal() {
    }

    /** Number of bytes a COMP-3 field with the given total digit count occupies. */
    public static int byteLength(int digits) {
        return digits / 2 + 1;
    }

    public static BigDecimal parse(byte[] bytes, int offset, int length, int scale) {
        StringBuilder digits = new StringBuilder(length * 2);
        for (int i = 0; i < length; i++) {
            int b = bytes[offset + i] & 0xFF;
            digits.append((char) ('0' + (b >> 4)));
            if (i < length - 1) {
                digits.append((char) ('0' + (b & 0x0F)));
            }
        }
        int signNibble = bytes[offset + length - 1] & 0x0F;
        BigInteger unscaled = new BigInteger(digits.toString());
        if (signNibble == 0x0D || signNibble == 0x0B) {
            unscaled = unscaled.negate();
        }
        return new BigDecimal(unscaled, scale);
    }

    public static byte[] format(BigDecimal value, int digits, int scale, boolean signed) {
        BigDecimal scaled = value.setScale(scale, RoundingMode.DOWN);
        BigInteger unscaled = scaled.unscaledValue();
        boolean negative = unscaled.signum() < 0;
        String s = unscaled.abs().toString();
        if (s.length() > digits) {
            s = s.substring(s.length() - digits);
        } else {
            s = "0".repeat(digits - s.length()) + s;
        }
        // Total nibbles = digits + 1 (sign); pad to an even count with a leading zero.
        if ((s.length() + 1) % 2 != 0) {
            s = "0" + s;
        }
        int len = (s.length() + 1) / 2;
        byte[] out = new byte[len];
        int nibbleIndex = 0;
        for (int i = 0; i < s.length(); i++) {
            int d = s.charAt(i) - '0';
            if (nibbleIndex % 2 == 0) {
                out[nibbleIndex / 2] = (byte) (d << 4);
            } else {
                out[nibbleIndex / 2] |= (byte) d;
            }
            nibbleIndex++;
        }
        int sign = !signed ? 0x0F : (negative ? 0x0D : 0x0C);
        out[len - 1] |= (byte) sign;
        return out;
    }
}
