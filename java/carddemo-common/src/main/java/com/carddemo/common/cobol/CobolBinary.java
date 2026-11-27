package com.carddemo.common.cobol;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/** COMP / COMP-4 / BINARY / COMP-5 codec: big-endian two's complement, sized by the picture's digit count. */
public final class CobolBinary {

    private CobolBinary() {
    }

    public static int byteLength(int digits) {
        if (digits <= 4) {
            return 2;
        }
        if (digits <= 9) {
            return 4;
        }
        if (digits <= 18) {
            return 8;
        }
        throw new CobolDataException("Binary picture with more than 18 digits is not supported: " + digits);
    }

    public static BigDecimal decode(byte[] buf, int offset, int length, int scale, boolean signed) {
        byte[] slice = new byte[length];
        System.arraycopy(buf, offset, slice, 0, length);
        BigInteger value = signed ? new BigInteger(slice) : new BigInteger(1, slice);
        return new BigDecimal(value, scale);
    }

    public static long decodeLong(byte[] buf, int offset, int length, boolean signed) {
        return decode(buf, offset, length, 0, signed).longValueExact();
    }

    /**
     * Encodes a binary item. With {@code nativeBinary=false} (COMP, TRUNC(STD)) the value must fit the picture
     * digits; with {@code nativeBinary=true} (COMP-5) it only has to fit the storage size.
     */
    public static void encode(BigDecimal value, int digits, int scale, boolean signed, boolean nativeBinary,
                              byte[] buf, int offset) {
        BigInteger unscaled = value.setScale(scale, RoundingMode.DOWN).unscaledValue();
        if (!signed) {
            unscaled = unscaled.abs();
        }
        int length = byteLength(digits);
        if (!nativeBinary && unscaled.abs().toString().length() > digits) {
            throw new CobolDataException("Value " + value + " overflows binary picture of " + digits + " digits");
        }
        int bits = length * 8;
        BigInteger min = signed ? BigInteger.ONE.shiftLeft(bits - 1).negate() : BigInteger.ZERO;
        BigInteger max = signed ? BigInteger.ONE.shiftLeft(bits - 1).subtract(BigInteger.ONE)
                : BigInteger.ONE.shiftLeft(bits).subtract(BigInteger.ONE);
        if (unscaled.compareTo(min) < 0 || unscaled.compareTo(max) > 0) {
            throw new CobolDataException("Value " + value + " overflows " + length + "-byte binary storage");
        }
        long bitsValue = unscaled.longValue();
        for (int i = length - 1; i >= 0; i--) {
            buf[offset + i] = (byte) (bitsValue & 0xFF);
            bitsValue >>>= 8;
        }
    }

    public static byte[] encode(BigDecimal value, int digits, int scale, boolean signed, boolean nativeBinary) {
        byte[] out = new byte[byteLength(digits)];
        encode(value, digits, scale, signed, nativeBinary, out, 0);
        return out;
    }
}
