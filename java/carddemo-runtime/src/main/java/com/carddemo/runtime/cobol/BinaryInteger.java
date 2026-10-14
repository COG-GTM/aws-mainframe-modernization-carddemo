package com.carddemo.runtime.cobol;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * COMP / COMP-4 / BINARY codec (big-endian two's complement, as on z/OS).
 * Storage size follows the picture: 1-4 digits = 2 bytes, 5-9 = 4 bytes, 10-18 = 8 bytes.
 * Values are truncated to the picture's digits on store (TRUNC(STD)).
 */
public final class BinaryInteger {

    private BinaryInteger() {
    }

    public static int length(int digits) {
        if (digits <= 4) {
            return 2;
        }
        if (digits <= 9) {
            return 4;
        }
        return 8;
    }

    public static byte[] encode(BigDecimal value, int integerDigits, int scale, boolean signed) {
        BigDecimal stored = CobolNumeric.store(value, integerDigits, scale, signed);
        int len = length(integerDigits + scale);
        long v = stored.unscaledValue().longValueExact();
        byte[] out = new byte[len];
        for (int i = len - 1; i >= 0; i--) {
            out[i] = (byte) (v & 0xFF);
            v >>= 8;
        }
        return out;
    }

    public static BigDecimal decode(byte[] bytes, int scale, boolean signed) {
        BigInteger v = signed ? new BigInteger(bytes) : new BigInteger(1, bytes);
        return new BigDecimal(v, scale);
    }
}
