package com.carddemo.recordio.codec;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * COBOL {@code USAGE DISPLAY} numeric semantics: zoned decimal with trailing sign overpunch, and
 * the silent high-order / low-order truncation that a MOVE or COMPUTE (without ROUNDED or
 * ON SIZE ERROR) applies when a value is stored into a {@code PIC S9(n)V9(m)} field.
 */
public final class CobolNumeric {

    private static final String POSITIVE_OVERPUNCH = "{ABCDEFGHI";
    private static final String NEGATIVE_OVERPUNCH = "}JKLMNOPQR";

    private CobolNumeric() {
    }

    /**
     * Decodes the character image of a zoned-decimal field.
     *
     * @param text   the field exactly as read (length = total digits)
     * @param scale  implied decimal places ({@code V99} = 2)
     * @param signed whether the PICTURE carries an {@code S}
     */
    public static BigDecimal decodeZoned(String text, int scale, boolean signed) {
        if (text.isEmpty()) {
            throw new RecordFormatException("empty numeric field");
        }
        StringBuilder digits = new StringBuilder(text.length());
        boolean negative = false;
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            boolean last = i == text.length() - 1;
            if (c >= '0' && c <= '9') {
                digits.append(c);
            } else if (last && signed && POSITIVE_OVERPUNCH.indexOf(c) >= 0) {
                digits.append((char) ('0' + POSITIVE_OVERPUNCH.indexOf(c)));
            } else if (last && signed && NEGATIVE_OVERPUNCH.indexOf(c) >= 0) {
                digits.append((char) ('0' + NEGATIVE_OVERPUNCH.indexOf(c)));
                negative = true;
            } else {
                throw new RecordFormatException(
                        "non-numeric character '" + c + "' at position " + i + " of zoned field '" + text + "'");
            }
        }
        BigInteger unscaled = new BigInteger(digits.toString());
        if (negative) {
            unscaled = unscaled.negate();
        }
        return new BigDecimal(unscaled, scale);
    }

    /**
     * Encodes a value into a zoned-decimal image of {@code digits} characters, applying COBOL
     * store semantics: fractional digits beyond {@code scale} are dropped (no ROUNDED) and
     * high-order digits that do not fit are dropped (no ON SIZE ERROR). Negative values stored
     * into an unsigned field lose their sign, as in COBOL.
     */
    public static String encodeZoned(BigDecimal value, int digits, int scale, boolean signed) {
        BigDecimal fitted = truncate(value, digits - scale, scale);
        boolean negative = signed && fitted.signum() < 0;
        String plain = fitted.abs().unscaledValue().toString();
        String padded = "0".repeat(Math.max(0, digits - plain.length())) + plain;
        if (!signed) {
            return padded;
        }
        int lastDigit = padded.charAt(digits - 1) - '0';
        char overpunch = negative ? NEGATIVE_OVERPUNCH.charAt(lastDigit) : POSITIVE_OVERPUNCH.charAt(lastDigit);
        return padded.substring(0, digits - 1) + overpunch;
    }

    /**
     * Applies the storage truncation of a {@code PIC S9(intDigits)V9(scale)} receiving field:
     * low-order decimals are cut (RoundingMode.DOWN) and high-order digits are cut modulo
     * 10^intDigits, sign preserved. This is what COMPUTE/ADD/MOVE do without ROUNDED and without
     * ON SIZE ERROR.
     */
    public static BigDecimal truncate(BigDecimal value, int intDigits, int scale) {
        BigDecimal scaled = value.setScale(scale, RoundingMode.DOWN);
        BigInteger limit = BigInteger.TEN.pow(intDigits + scale);
        BigInteger unscaled = scaled.unscaledValue();
        BigInteger fitted = unscaled.abs().mod(limit);
        if (unscaled.signum() < 0) {
            fitted = fitted.negate();
        }
        return new BigDecimal(fitted, scale);
    }

    /** True when the value can be stored in {@code PIC S9(intDigits)V9(scale)} without loss. */
    public static boolean fits(BigDecimal value, int intDigits, int scale) {
        return truncate(value, intDigits, scale).compareTo(value) == 0;
    }
}
