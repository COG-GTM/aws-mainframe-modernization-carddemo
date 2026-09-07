package com.carddemo.poc.copybook;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Encoding/decoding of COBOL zoned-decimal (USAGE DISPLAY) numeric fields such as
 * {@code PIC 9(11)} or {@code PIC S9(10)V99}.
 *
 * <p>Each digit occupies one character. For signed pictures the sign is "overpunched"
 * into the zone nibble of the last digit. After EBCDIC->ASCII code-page translation
 * (CP037 -> ISO-8859-1) the overpunched characters are:
 * <pre>
 *   positive:  { A B C D E F G H I   (0..9)
 *   negative:  } J K L M N O P Q R   (0..9)
 * </pre>
 * The sample data in {@code app/data} uses exactly this convention
 * (e.g. {@code 00000001940{} = +19.40 for {@code PIC S9(10)V99}).
 */
public final class ZonedDecimal {

    private static final String POSITIVE_OVERPUNCH = "{ABCDEFGHI";
    private static final String NEGATIVE_OVERPUNCH = "}JKLMNOPQR";

    private ZonedDecimal() {
    }

    /**
     * Parses a zoned decimal field.
     *
     * @param text   the raw characters of the field, exactly {@code digits} long
     * @param scale  number of implied decimal places (the {@code V99} part)
     * @param signed whether the picture is signed ({@code S9...})
     */
    public static BigDecimal parse(String text, int scale, boolean signed) {
        if (text == null || text.isEmpty()) {
            throw new IllegalArgumentException("empty zoned decimal");
        }
        String digits = text;
        boolean negative = false;
        if (signed) {
            char last = text.charAt(text.length() - 1);
            int pos = POSITIVE_OVERPUNCH.indexOf(last);
            int neg = NEGATIVE_OVERPUNCH.indexOf(last);
            if (pos >= 0) {
                digits = text.substring(0, text.length() - 1) + (char) ('0' + pos);
            } else if (neg >= 0) {
                negative = true;
                digits = text.substring(0, text.length() - 1) + (char) ('0' + neg);
            } else if (last == '-') {
                negative = true;
                digits = text.substring(0, text.length() - 1) + '0';
            }
            // An unsigned trailing digit is also accepted (sign nibble F = unsigned positive).
        }
        for (int i = 0; i < digits.length(); i++) {
            char c = digits.charAt(i);
            if (c == ' ') {
                // COBOL treats leading spaces in numeric DISPLAY fields as zero on most compilers.
                digits = digits.substring(0, i) + '0' + digits.substring(i + 1);
            } else if (c < '0' || c > '9') {
                throw new NumberFormatException("invalid zoned decimal digit '" + c + "' in \"" + text + "\"");
            }
        }
        BigInteger unscaled = new BigInteger(digits);
        if (negative) {
            unscaled = unscaled.negate();
        }
        return new BigDecimal(unscaled, scale);
    }

    /** Convenience for {@code PIC 9(n)} unsigned integer fields. */
    public static long parseUnsignedLong(String text) {
        return parse(text, 0, false).longValueExact();
    }

    /**
     * Formats a value as a zoned decimal field of the given total digit count.
     * Truncates high-order digits (COBOL MOVE semantics) and the fraction to {@code scale}.
     */
    public static String format(BigDecimal value, int digits, int scale, boolean signed) {
        BigDecimal scaled = value.setScale(scale, java.math.RoundingMode.DOWN);
        BigInteger unscaled = scaled.unscaledValue();
        boolean negative = unscaled.signum() < 0;
        String s = unscaled.abs().toString();
        if (s.length() > digits) {
            s = s.substring(s.length() - digits);
        } else {
            s = "0".repeat(digits - s.length()) + s;
        }
        if (!signed) {
            return s;
        }
        int lastDigit = s.charAt(s.length() - 1) - '0';
        char overpunch = (negative ? NEGATIVE_OVERPUNCH : POSITIVE_OVERPUNCH).charAt(lastDigit);
        return s.substring(0, s.length() - 1) + overpunch;
    }
}
