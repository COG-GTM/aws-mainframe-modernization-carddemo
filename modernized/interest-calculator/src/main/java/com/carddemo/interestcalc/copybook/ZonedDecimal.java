package com.carddemo.interestcalc.copybook;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * Codec for COBOL signed {@code DISPLAY} (zoned decimal) fields as they appear in the
 * CardDemo ASCII datasets, i.e. {@code PIC S9(n)V99} with a trailing EBCDIC sign overpunch.
 *
 * <p>The sign is carried in the last byte: {@code '{'..'I'} encode the digits 0..9 of a
 * positive value and {@code '}'..'R'} the digits 0..9 of a negative value. Neither the sign
 * nor the decimal point occupy a byte of their own, so {@code PIC S9(9)V99} is 11 bytes.
 *
 * <p>Never uses {@code double}: every value crosses this boundary as a {@link BigDecimal}
 * with an explicit scale.
 */
public final class ZonedDecimal {

    private static final String POSITIVE_OVERPUNCH = "{ABCDEFGHI";
    private static final String NEGATIVE_OVERPUNCH = "}JKLMNOPQR";

    private ZonedDecimal() {
    }

    /**
     * Decodes a zoned decimal field.
     *
     * @param field the raw bytes of the field, exactly {@code intDigits + decDigits} characters
     * @param intDigits digits before the implied decimal point
     * @param decDigits digits after the implied decimal point
     */
    public static BigDecimal decode(String field, int intDigits, int decDigits) {
        int width = intDigits + decDigits;
        if (field.length() != width) {
            throw new IllegalArgumentException(
                    "expected " + width + " bytes for PIC S9(" + intDigits + ")V9(" + decDigits + ") but got '" + field + "'");
        }
        StringBuilder digits = new StringBuilder(width);
        boolean negative = false;
        for (int i = 0; i < width; i++) {
            char c = field.charAt(i);
            boolean last = i == width - 1;
            if (c >= '0' && c <= '9') {
                digits.append(c);
            } else if (last && POSITIVE_OVERPUNCH.indexOf(c) >= 0) {
                digits.append((char) ('0' + POSITIVE_OVERPUNCH.indexOf(c)));
            } else if (last && NEGATIVE_OVERPUNCH.indexOf(c) >= 0) {
                digits.append((char) ('0' + NEGATIVE_OVERPUNCH.indexOf(c)));
                negative = true;
            } else if (c == ' ') {
                // Low-values/spaces in a numeric DISPLAY field read as zero on the mainframe.
                digits.append('0');
            } else {
                throw new IllegalArgumentException("invalid zoned decimal byte '" + c + "' in field '" + field + "'");
            }
        }
        BigInteger unscaled = new BigInteger(digits.toString());
        if (negative) {
            unscaled = unscaled.negate();
        }
        return new BigDecimal(unscaled, decDigits);
    }

    /**
     * Encodes a value back into the fixed-width zoned decimal representation, applying the
     * COBOL storage semantics of the receiving field: truncation (never rounding) of excess
     * decimal places and silent truncation of excess high-order digits.
     */
    public static String encode(BigDecimal value, int intDigits, int decDigits) {
        BigDecimal stored = CobolNumeric.store(value, intDigits, decDigits);
        int width = intDigits + decDigits;
        String digits = stored.abs().setScale(decDigits, RoundingMode.DOWN)
                .unscaledValue().toString();
        digits = "0".repeat(Math.max(0, width - digits.length())) + digits;
        String table = stored.signum() < 0 ? NEGATIVE_OVERPUNCH : POSITIVE_OVERPUNCH;
        char lastDigit = digits.charAt(width - 1);
        return digits.substring(0, width - 1) + table.charAt(lastDigit - '0');
    }
}
