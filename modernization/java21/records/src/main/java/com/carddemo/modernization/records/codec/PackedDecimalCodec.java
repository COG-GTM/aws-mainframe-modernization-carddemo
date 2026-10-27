package com.carddemo.modernization.records.codec;

import java.math.BigDecimal;
import java.math.BigInteger;

/** COMP-3; C/D output, F accepted on input. */
public final class PackedDecimalCodec {
    private final int digits;
    private final int scale;
    public PackedDecimalCodec(int digits, int scale) {
        if (digits < 1 || scale < 0 || scale > digits) throw new IllegalArgumentException("Invalid PIC");
        this.digits = digits; this.scale = scale;
    }
    public byte[] encode(BigDecimal value) {
        String nibbles = ZonedDecimalCodec.magnitude(value, digits, scale) + (value.signum() < 0 ? "D" : "C");
        if (nibbles.length() % 2 != 0) nibbles = "0" + nibbles;
        return java.util.HexFormat.of().parseHex(nibbles);
    }
    public BigDecimal decode(byte[] bytes) {
        if (bytes.length != (digits + 2) / 2) throw new IllegalArgumentException("Wrong packed width");
        String text = java.util.HexFormat.of().withUpperCase().formatHex(bytes);
        int start = text.length() - digits - 1;
        if (start == 1 && text.charAt(0) != '0') throw new IllegalArgumentException("Nonzero padding nibble");
        char sign = text.charAt(text.length() - 1);
        if (sign != 'C' && sign != 'D' && sign != 'F') throw new IllegalArgumentException("Invalid packed sign");
        String magnitude = text.substring(start, text.length() - 1);
        ZonedDecimalCodec.requireDigits(magnitude);
        BigDecimal value = new BigDecimal(new BigInteger(magnitude), scale);
        return sign == 'D' ? value.negate() : value;
    }
}
