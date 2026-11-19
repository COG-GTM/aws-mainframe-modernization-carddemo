package com.carddemo.common.cobol;

import com.carddemo.common.error.NumericDataException;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * {@code COMP} / {@code BINARY}: big-endian, two's complement when signed. Storage is 2 bytes for
 * 1-4 digits, 4 bytes for 5-9 and 8 bytes for 10-18. Decoding enforces the picture's digit limit
 * ({@code TRUNC(STD)}); a value beyond it is invalid data.
 */
public final class BinaryInteger {

    private BinaryInteger() {
    }

    public static int byteLength(int digits) {
        if (digits < 1 || digits > 18) {
            throw new IllegalArgumentException("COMP supports 1-18 digits: " + digits);
        }
        return digits <= 4 ? 2 : digits <= 9 ? 4 : 8;
    }

    public static BigDecimal decode(byte[] bytes, int offset, int length, int digits, int scale, boolean signed) {
        if (length != byteLength(digits)) {
            throw new IllegalArgumentException("PIC 9(" + digits + ") COMP is " + byteLength(digits) + " bytes, got " + length);
        }
        byte[] slice = java.util.Arrays.copyOfRange(bytes, offset, offset + length);
        BigInteger unscaled = signed ? new BigInteger(slice) : new BigInteger(1, slice);
        if (unscaled.abs().compareTo(BigInteger.TEN.pow(digits)) >= 0) {
            throw new NumericDataException("COMP value " + unscaled + " exceeds PIC 9(" + digits + ")");
        }
        return new BigDecimal(unscaled, scale);
    }

    public static BigDecimal decode(byte[] bytes, int digits, int scale, boolean signed) {
        return decode(bytes, 0, bytes.length, digits, scale, signed);
    }

    /** Encodes {@code value} (which must fit {@code PIC S9(integerDigits)V9(scale)}) into {@link #byteLength} bytes. */
    public static byte[] encode(BigDecimal value, int integerDigits, int scale, boolean signed) {
        if (!CobolDecimal.fits(value, integerDigits, scale)) {
            throw new IllegalArgumentException(value + " does not fit PIC " + (signed ? "S" : "")
                    + "9(" + integerDigits + ")V9(" + scale + ") COMP");
        }
        if (!signed && value.signum() < 0) {
            throw new IllegalArgumentException("Negative value " + value + " for an unsigned field");
        }
        int length = byteLength(integerDigits + scale);
        byte[] raw = value.setScale(scale).unscaledValue().toByteArray();
        byte[] out = new byte[length];
        byte fill = (byte) (value.signum() < 0 ? 0xFF : 0x00);
        java.util.Arrays.fill(out, fill);
        int copy = Math.min(raw.length, length);
        System.arraycopy(raw, raw.length - copy, out, length - copy, copy);
        return out;
    }
}
