package com.carddemo.common.cobol;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * COMP-3 / PACKED-DECIMAL codec. Two digits per byte, sign in the low nibble of the last byte
 * (C/A/E/F positive, D/B negative, F written for unsigned pictures).
 */
public final class PackedDecimal {

    private static final int POSITIVE = 0x0C;
    private static final int NEGATIVE = 0x0D;
    private static final int UNSIGNED = 0x0F;

    private PackedDecimal() {
    }

    /** Storage bytes for a picture with {@code digits} digit positions (e.g. S9(10)V99 -> 12 digits -> 7 bytes). */
    public static int byteLength(int digits) {
        return digits / 2 + 1;
    }

    public static BigDecimal decode(byte[] buf, int offset, int length, int scale) {
        if (length <= 0 || length > 16) {
            throw new CobolDataException("Packed decimal length out of range: " + length);
        }
        long acc = 0;
        BigInteger big = null;
        int digits = 0;
        for (int i = 0; i < length; i++) {
            int b = buf[offset + i] & 0xFF;
            int hi = b >>> 4;
            int lo = b & 0x0F;
            int[] nibbles = i < length - 1 ? new int[] {hi, lo} : new int[] {hi};
            for (int d : nibbles) {
                if (d > 9) {
                    throw new CobolDataException(invalid(buf, offset, length, "digit nibble " + Integer.toHexString(d)));
                }
                if (++digits > 18 && big == null) {
                    big = BigInteger.valueOf(acc);
                }
                if (big != null) {
                    big = big.multiply(BigInteger.TEN).add(BigInteger.valueOf(d));
                } else {
                    acc = acc * 10 + d;
                }
            }
        }
        int sign = buf[offset + length - 1] & 0x0F;
        boolean negative = switch (sign) {
            case 0x0D, 0x0B -> true;
            case 0x0C, 0x0A, 0x0E, 0x0F -> false;
            default -> throw new CobolDataException(invalid(buf, offset, length, "sign nibble " + Integer.toHexString(sign)));
        };
        BigInteger unscaled = big != null ? big : BigInteger.valueOf(acc);
        return new BigDecimal(negative ? unscaled.negate() : unscaled, scale);
    }

    public static boolean isValid(byte[] buf, int offset, int length) {
        try {
            decode(buf, offset, length, 0);
            return true;
        } catch (CobolDataException e) {
            return false;
        }
    }

    public static byte[] encode(BigDecimal value, int digits, int scale, boolean signed) {
        byte[] out = new byte[byteLength(digits)];
        encode(value, digits, scale, signed, out, 0);
        return out;
    }

    /**
     * Encodes with COBOL MOVE semantics: excess decimal places are truncated, a negative value moved to an
     * unsigned picture keeps its magnitude. Integer-part overflow is rejected instead of silently truncated.
     */
    public static void encode(BigDecimal value, int digits, int scale, boolean signed, byte[] buf, int offset) {
        BigInteger unscaled = value.setScale(scale, RoundingMode.DOWN).unscaledValue();
        boolean negative = unscaled.signum() < 0;
        String text = unscaled.abs().toString();
        if (text.length() > digits) {
            throw new CobolDataException("Value " + value + " overflows packed picture of " + digits + " digits");
        }
        int length = byteLength(digits);
        int nibbleCount = length * 2 - 1;
        String padded = "0".repeat(nibbleCount - text.length()) + text;
        int sign = !signed ? UNSIGNED : negative ? NEGATIVE : POSITIVE;
        for (int i = 0; i < length; i++) {
            int hi = padded.charAt(i * 2) - '0';
            int lo = i < length - 1 ? padded.charAt(i * 2 + 1) - '0' : sign;
            buf[offset + i] = (byte) ((hi << 4) | lo);
        }
    }

    private static String invalid(byte[] buf, int offset, int length, String what) {
        return "Invalid packed decimal (" + what + "): " + Hex.of(buf, offset, length);
    }
}
