package com.carddemo.modernization.records.codec;

import java.math.BigInteger;

/** Signed big-endian COMP: 1-4/5-9/10-18 PIC digits use 2/4/8 bytes. */
public final class BinaryCodec {
    private final int width;
    private final BigInteger limit;
    public BinaryCodec(int digits) {
        if (digits < 1 || digits > 18) throw new IllegalArgumentException("Invalid COMP digits");
        width = digits <= 4 ? 2 : digits <= 9 ? 4 : 8;
        limit = BigInteger.TEN.pow(digits);
    }
    public byte[] encode(long value) {
        validate(BigInteger.valueOf(value));
        byte[] bytes = new byte[width];
        for (int i = width - 1; i >= 0; i--) { bytes[i] = (byte) value; value >>= 8; }
        return bytes;
    }
    public long decode(byte[] bytes) {
        if (bytes.length != width) throw new IllegalArgumentException("Wrong COMP width");
        BigInteger value = new BigInteger(bytes);
        validate(value);
        return value.longValueExact();
    }
    private void validate(BigInteger value) {
        if (value.abs().compareTo(limit) >= 0) throw new IllegalArgumentException("COMP PIC overflow");
    }
}
