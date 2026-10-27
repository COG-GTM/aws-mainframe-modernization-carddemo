package com.carddemo.modernization.records.codec;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.Charset;

/** Trailing ASCII overpunch; IBM037 uses the corresponding C/D zoned sign bytes. */
public final class ZonedDecimalCodec {
    private static final String POSITIVE = "{ABCDEFGHI";
    private static final String NEGATIVE = "}JKLMNOPQR";
    private final int digits;
    private final int scale;

    public ZonedDecimalCodec(int digits, int scale) {
        if (digits < 1 || scale < 0 || scale > digits) throw new IllegalArgumentException("Invalid PIC");
        this.digits = digits;
        this.scale = scale;
    }

    public ZonedDecimal decode(byte[] bytes, Charset charset) {
        String text = TextCodec.decode(bytes, charset);
        if (text.length() != digits) throw new IllegalArgumentException("Wrong zoned width");
        char last = text.charAt(digits - 1);
        int digit = POSITIVE.indexOf(last);
        boolean negative = false;
        if (digit < 0) { digit = NEGATIVE.indexOf(last); negative = true; }
        if (digit < 0) throw new IllegalArgumentException("Invalid overpunch");
        String magnitude = text.substring(0, digits - 1) + digit;
        requireDigits(magnitude);
        BigDecimal value = new BigDecimal(new BigInteger(magnitude), scale);
        return new ZonedDecimal(negative ? value.negate() : value, negative && value.signum() == 0);
    }

    public byte[] encode(ZonedDecimal number, Charset charset) {
        String magnitude = magnitude(number.value(), digits, scale);
        boolean negative = number.value().signum() < 0 || number.negativeZero();
        String text = magnitude.substring(0, digits - 1)
                + (negative ? NEGATIVE : POSITIVE).charAt(magnitude.charAt(digits - 1) - '0');
        return TextCodec.encode(text, digits, charset);
    }

    public static String unsigned(String text, int digits) {
        if (text.length() != digits) throw new IllegalArgumentException("Wrong unsigned width");
        requireDigits(text);
        return text;
    }

    static void requireDigits(String text) {
        if (text.isEmpty() || !text.chars().allMatch(c -> c >= '0' && c <= '9'))
            throw new IllegalArgumentException("Invalid decimal digits");
    }

    static String magnitude(BigDecimal value, int digits, int scale) {
        if (value.scale() != scale) throw new IllegalArgumentException("Wrong PIC scale");
        String text = value.unscaledValue().abs().toString();
        if (text.length() > digits) throw new IllegalArgumentException("Decimal overflow");
        return "0".repeat(digits - text.length()) + text;
    }
}
