package com.carddemo.runtime.cobol;

import java.math.BigDecimal;
import java.math.BigInteger;

/** COMP-3 (packed decimal) codec: two digits per byte, trailing sign nibble C (+), D (-), F (unsigned). */
public final class PackedDecimal {

    private PackedDecimal() {
    }

    /** Bytes occupied by a COMP-3 item with the given total number of digits. */
    public static int length(int integerDigits, int scale) {
        return (integerDigits + scale) / 2 + 1;
    }

    public static byte[] encode(BigDecimal value, int integerDigits, int scale, boolean signed) {
        BigDecimal stored = CobolNumeric.store(value, integerDigits, scale, signed);
        int len = length(integerDigits, scale);
        int nibbles = len * 2 - 1;
        String digits = stored.unscaledValue().abs().toString();
        StringBuilder sb = new StringBuilder(nibbles);
        for (int i = digits.length(); i < nibbles; i++) {
            sb.append('0');
        }
        sb.append(digits);
        byte[] out = new byte[len];
        for (int i = 0; i < len; i++) {
            int hi = sb.charAt(i * 2) - '0';
            int lo;
            if (i == len - 1) {
                lo = !signed ? 0xF : (stored.signum() < 0 ? 0xD : 0xC);
            } else {
                lo = sb.charAt(i * 2 + 1) - '0';
            }
            out[i] = (byte) ((hi << 4) | lo);
        }
        return out;
    }

    /** Decodes packed data. Invalid digit or sign nibbles throw {@link NumberFormatException} (S0C7). */
    public static BigDecimal decode(byte[] packed, int scale) {
        StringBuilder sb = new StringBuilder(packed.length * 2);
        boolean negative = false;
        for (int i = 0; i < packed.length; i++) {
            int b = packed[i] & 0xFF;
            int hi = b >>> 4;
            int lo = b & 0x0F;
            if (hi > 9) {
                throw new NumberFormatException("Invalid packed digit nibble at byte " + i);
            }
            sb.append((char) ('0' + hi));
            if (i == packed.length - 1) {
                if (lo == 0xD || lo == 0xB) {
                    negative = true;
                } else if (lo < 0xA) {
                    throw new NumberFormatException("Invalid packed sign nibble " + Integer.toHexString(lo));
                }
            } else {
                if (lo > 9) {
                    throw new NumberFormatException("Invalid packed digit nibble at byte " + i);
                }
                sb.append((char) ('0' + lo));
            }
        }
        BigInteger unscaled = new BigInteger(sb.toString());
        return new BigDecimal(negative ? unscaled.negate() : unscaled, scale);
    }

    public static boolean isNumeric(byte[] packed) {
        try {
            decode(packed, 0);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }
}
