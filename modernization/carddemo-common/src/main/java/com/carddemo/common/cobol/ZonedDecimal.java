package com.carddemo.common.cobol;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Display (zoned) numerics as GnuCOBOL {@code -fsign=EBCDIC} reads and writes them in ASCII files: the sign is
 * overpunched on the last digit, {@code '{','A'..'I'} = +0..+9} and {@code '}','J'..'R'} = -0..-9}. Unsigned
 * {@code PIC 9(n)} fields are plain digits. A plain last digit in a signed field is read as positive.
 */
public final class ZonedDecimal {

    private static final String POSITIVE = "{ABCDEFGHI";
    private static final String NEGATIVE = "}JKLMNOPQR";

    private ZonedDecimal() {
    }

    /** Decodes {@code PIC S9(digits-scale)V9(scale)} or {@code 9(n)} text into a value with the given scale. */
    public static BigDecimal decode(String text, int scale, boolean signed) {
        if (text.isEmpty()) {
            throw new NumberFormatException("empty zoned field");
        }
        StringBuilder digits = new StringBuilder(text.length());
        boolean negative = false;
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            boolean last = i == text.length() - 1;
            if (c >= '0' && c <= '9') {
                digits.append(c);
            } else if (last && signed && POSITIVE.indexOf(c) >= 0) {
                digits.append((char) ('0' + POSITIVE.indexOf(c)));
            } else if (last && signed && NEGATIVE.indexOf(c) >= 0) {
                digits.append((char) ('0' + NEGATIVE.indexOf(c)));
                negative = true;
            } else {
                throw new NumberFormatException("not a " + (signed ? "signed" : "unsigned") + " zoned field: '" + text + "'");
            }
        }
        BigInteger unscaled = new BigInteger(digits.toString());
        return new BigDecimal(negative ? unscaled.negate() : unscaled, scale);
    }

    /** Encodes into exactly {@code length} characters; refuses values that do not fit instead of truncating. */
    public static String encode(BigDecimal value, int length, int scale, boolean signed) {
        BigInteger unscaled;
        try {
            unscaled = value.setScale(scale).unscaledValue();
        } catch (ArithmeticException e) {
            throw new IllegalArgumentException(value + " has more than " + scale + " decimals", e);
        }
        if (unscaled.signum() < 0 && !signed) {
            throw new IllegalArgumentException(value + " is negative for an unsigned field");
        }
        String digits = unscaled.abs().toString();
        if (digits.length() > length) {
            throw new IllegalArgumentException(value + " does not fit in " + length + " digits");
        }
        String padded = "0".repeat(length - digits.length()) + digits;
        if (!signed) {
            return padded;
        }
        int last = padded.charAt(length - 1) - '0';
        char sign = unscaled.signum() < 0 ? NEGATIVE.charAt(last) : POSITIVE.charAt(last);
        return padded.substring(0, length - 1) + sign;
    }
}
