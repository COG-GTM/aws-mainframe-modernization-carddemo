package com.carddemo.transaction.cobol;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * USAGE COMP-3 (packed decimal): two digits per byte, sign in the low nibble of the last byte
 * (C/A/E/F = positive, D/B = negative). Used when reading binary (EBCDIC) extracts.
 */
public final class PackedDecimal {

    private PackedDecimal() {
    }

    public static BigDecimal decode(byte[] bytes, int scale) {
        if (bytes == null || bytes.length == 0) {
            throw new CobolDataException("empty COMP-3 field");
        }
        StringBuilder digits = new StringBuilder(bytes.length * 2);
        for (int i = 0; i < bytes.length; i++) {
            int hi = (bytes[i] >> 4) & 0x0F;
            int lo = bytes[i] & 0x0F;
            appendDigit(digits, hi, bytes);
            if (i < bytes.length - 1) {
                appendDigit(digits, lo, bytes);
            } else {
                BigInteger unscaled = new BigInteger(digits.toString());
                return new BigDecimal(isNegative(lo, bytes) ? unscaled.negate() : unscaled, scale);
            }
        }
        throw new IllegalStateException("unreachable");
    }

    /** Encodes a value for {@code PIC S9(integerDigits)V9(scale) COMP-3}. */
    public static byte[] encode(BigDecimal value, int integerDigits, int scale) {
        BigDecimal v = Pic.s9(value, integerDigits, scale);
        int totalDigits = integerDigits + scale;
        int nibbles = totalDigits % 2 == 0 ? totalDigits + 2 : totalDigits + 1;
        String digits = v.unscaledValue().abs().toString();
        digits = "0".repeat(nibbles - 1 - digits.length()) + digits;
        byte[] out = new byte[nibbles / 2];
        for (int i = 0; i < out.length; i++) {
            int hi = digits.charAt(i * 2) - '0';
            int lo = i == out.length - 1 ? (v.signum() < 0 ? 0x0D : 0x0C) : digits.charAt(i * 2 + 1) - '0';
            out[i] = (byte) ((hi << 4) | lo);
        }
        return out;
    }

    private static void appendDigit(StringBuilder sb, int nibble, byte[] field) {
        if (nibble > 9) {
            throw new CobolDataException("invalid digit nibble in COMP-3 field " + hex(field));
        }
        sb.append((char) ('0' + nibble));
    }

    private static boolean isNegative(int signNibble, byte[] field) {
        return switch (signNibble) {
            case 0x0C, 0x0A, 0x0E, 0x0F -> false;
            case 0x0D, 0x0B -> true;
            default -> throw new CobolDataException("invalid sign nibble in COMP-3 field " + hex(field));
        };
    }

    private static String hex(byte[] bytes) {
        StringBuilder sb = new StringBuilder();
        for (byte b : bytes) {
            sb.append(String.format("%02X", b));
        }
        return sb.toString();
    }
}
