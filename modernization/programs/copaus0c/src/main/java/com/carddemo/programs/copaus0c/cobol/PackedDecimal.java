package com.carddemo.programs.copaus0c.cobol;

import java.math.BigDecimal;
import java.math.BigInteger;

/** COMP-3 (packed decimal) encode/decode. */
public final class PackedDecimal {

    private PackedDecimal() {
    }

    public static int byteLength(int digits) {
        return digits / 2 + 1;
    }

    public static BigDecimal decode(byte[] data, int offset, int digits, int scale) {
        int len = byteLength(digits);
        StringBuilder sb = new StringBuilder(len * 2);
        for (int i = 0; i < len; i++) {
            int b = data[offset + i] & 0xFF;
            sb.append((char) ('0' + (b >> 4)));
            if (i < len - 1) {
                sb.append((char) ('0' + (b & 0x0F)));
            }
        }
        int sign = data[offset + len - 1] & 0x0F;
        for (int i = 0; i < sb.length(); i++) {
            if (sb.charAt(i) > '9') {
                throw new IllegalArgumentException("Invalid packed digit at offset " + offset);
            }
        }
        if (sign < 0x0A) {
            throw new IllegalArgumentException("Invalid packed sign nibble at offset " + offset);
        }
        BigInteger unscaled = new BigInteger(sb.toString());
        if (sign == 0x0B || sign == 0x0D) {
            unscaled = unscaled.negate();
        }
        return new BigDecimal(unscaled, scale);
    }

    public static long decodeLong(byte[] data, int offset, int digits) {
        return decode(data, offset, digits, 0).longValueExact();
    }

    /** Encodes a signed value (preferred sign C/D) truncating high-order digits to {@code digits}. */
    public static byte[] encode(long value, int digits) {
        int len = byteLength(digits);
        String s = BigInteger.valueOf(value).abs().mod(BigInteger.TEN.pow(digits)).toString();
        s = "0".repeat(len * 2 - 1 - s.length()) + s;
        byte[] out = new byte[len];
        for (int i = 0; i < len; i++) {
            int hi = s.charAt(i * 2) - '0';
            int lo = i == len - 1 ? (value < 0 ? 0x0D : 0x0C) : s.charAt(i * 2 + 1) - '0';
            out[i] = (byte) ((hi << 4) | lo);
        }
        return out;
    }
}
