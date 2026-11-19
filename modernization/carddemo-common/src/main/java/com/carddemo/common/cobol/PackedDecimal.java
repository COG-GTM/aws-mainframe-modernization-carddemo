package com.carddemo.common.cobol;

import com.carddemo.common.error.NumericDataException;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * {@code COMP-3} packed decimal: two digits per byte, the sign in the low nibble of the last byte
 * (C, A, E, F positive; D, B negative). Written with C (signed positive), D (negative) or F (unsigned),
 * as IBM COBOL does. Length is {@code digits / 2 + 1} bytes. Packed bytes are binary: they are never
 * code-page converted.
 */
public final class PackedDecimal {

    private PackedDecimal() {
    }

    public static int byteLength(int digits) {
        return digits / 2 + 1;
    }

    public static BigDecimal decode(byte[] bytes, int scale) {
        return decode(bytes, 0, bytes.length, scale);
    }

    public static BigDecimal decode(byte[] bytes, int offset, int length, int scale) {
        if (length < 1) {
            throw new NumericDataException("Empty packed decimal field");
        }
        StringBuilder digits = new StringBuilder(length * 2);
        for (int i = 0; i < length; i++) {
            int b = bytes[offset + i] & 0xFF;
            int high = b >>> 4;
            int low = b & 0x0F;
            digits.append(digit(high, bytes, offset, length));
            if (i < length - 1) {
                digits.append(digit(low, bytes, offset, length));
            } else {
                boolean negative = switch (low) {
                    case 0x0C, 0x0A, 0x0E, 0x0F -> false;
                    case 0x0D, 0x0B -> true;
                    default -> throw new NumericDataException("Invalid packed sign nibble " + Integer.toHexString(low)
                            + " in " + hex(bytes, offset, length));
                };
                BigDecimal value = new BigDecimal(new BigInteger(digits.toString()), scale);
                return negative ? value.negate() : value;
            }
        }
        throw new IllegalStateException("unreachable");
    }

    /** Encodes into {@link #byteLength}{@code (integerDigits + scale)} bytes; the value must fit the picture. */
    public static byte[] encode(BigDecimal value, int integerDigits, int scale, boolean signed) {
        if (!CobolDecimal.fits(value, integerDigits, scale)) {
            throw new IllegalArgumentException(value + " does not fit PIC " + (signed ? "S" : "")
                    + "9(" + integerDigits + ")V9(" + scale + ") COMP-3");
        }
        if (!signed && value.signum() < 0) {
            throw new IllegalArgumentException("Negative value " + value + " for an unsigned field");
        }
        BigDecimal scaled = value.setScale(scale);
        int digitsCount = integerDigits + scale;
        int length = byteLength(digitsCount);
        String digits = scaled.unscaledValue().abs().toString();
        digits = "0".repeat(length * 2 - 1 - digits.length()) + digits;
        int sign = !signed ? 0x0F : scaled.signum() < 0 ? 0x0D : 0x0C;
        byte[] out = new byte[length];
        for (int i = 0; i < length; i++) {
            int high = digits.charAt(i * 2) - '0';
            int low = i < length - 1 ? digits.charAt(i * 2 + 1) - '0' : sign;
            out[i] = (byte) ((high << 4) | low);
        }
        return out;
    }

    private static char digit(int nibble, byte[] bytes, int offset, int length) {
        if (nibble > 9) {
            throw new NumericDataException("Invalid packed digit nibble " + Integer.toHexString(nibble)
                    + " in " + hex(bytes, offset, length));
        }
        return (char) ('0' + nibble);
    }

    static String hex(byte[] bytes, int offset, int length) {
        StringBuilder sb = new StringBuilder("X'");
        for (int i = 0; i < length; i++) {
            sb.append(String.format("%02X", bytes[offset + i] & 0xFF));
        }
        return sb.append('\'').toString();
    }
}
