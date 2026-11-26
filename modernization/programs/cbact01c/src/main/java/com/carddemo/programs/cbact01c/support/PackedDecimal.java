package com.carddemo.programs.cbact01c.support;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.Locale;

/** USAGE COMP-3 (packed decimal): two digits per byte, sign in the low nibble of the last byte (C=+, D=-, F=unsigned). */
public final class PackedDecimal {
    private PackedDecimal() {
    }

    public static int byteLength(int totalDigits) {
        return totalDigits / 2 + 1;
    }

    public static byte[] encode(BigDecimal value, int intDigits, int scale) {
        int total = intDigits + scale;
        int len = byteLength(total);
        BigInteger unscaled = value.setScale(scale, RoundingMode.DOWN).unscaledValue();
        boolean negative = unscaled.signum() < 0;
        BigInteger magnitude = unscaled.abs().mod(BigInteger.TEN.pow(total));
        String digits = String.format(Locale.ROOT, "%0" + (len * 2 - 1) + "d", magnitude);
        byte[] out = new byte[len];
        for (int i = 0; i < len; i++) {
            int hi = digits.charAt(i * 2) - '0';
            int lo = i == len - 1 ? (negative ? 0x0D : 0x0C) : digits.charAt(i * 2 + 1) - '0';
            out[i] = (byte) ((hi << 4) | lo);
        }
        return out;
    }

    public static BigDecimal decode(byte[] field, int scale) {
        StringBuilder digits = new StringBuilder(field.length * 2);
        for (int i = 0; i < field.length; i++) {
            int b = field[i] & 0xFF;
            int hi = b >> 4;
            int lo = b & 0x0F;
            if (hi > 9) {
                throw new DataException("invalid packed digit at byte " + i);
            }
            digits.append((char) ('0' + hi));
            if (i < field.length - 1) {
                if (lo > 9) {
                    throw new DataException("invalid packed digit at byte " + i);
                }
                digits.append((char) ('0' + lo));
            } else if (lo < 0x0A) {
                throw new DataException("invalid packed sign nibble " + Integer.toHexString(lo));
            }
        }
        int sign = field[field.length - 1] & 0x0F;
        BigInteger unscaled = new BigInteger(digits.toString());
        boolean negative = sign == 0x0B || sign == 0x0D;
        return new BigDecimal(negative ? unscaled.negate() : unscaled, scale);
    }
}
