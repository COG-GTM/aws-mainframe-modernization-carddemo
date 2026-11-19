package com.carddemo.common.cobol;

import com.carddemo.common.error.NumericDataException;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * USAGE DISPLAY numerics ({@code PIC 9(n)V9(s)} / {@code PIC S9(n)V9(s)}, sign trailing, overpunched).
 * Works on the decoded characters, so one rule covers both copies of the data: in CP037 the sign zone
 * C/D of the last byte decodes to {@code '{'}, {@code 'A'}-{@code 'I'} (+0..+9) and {@code '}'},
 * {@code 'J'}-{@code 'R'} (-0..-9); the {@code app/data/ASCII} files keep those same characters
 * (e.g. DALYTRAN-AMT {@code 0000005047G} = +504.77). An unsigned digit (zone F) is positive.
 */
public final class ZonedDecimal {

    private ZonedDecimal() {
    }

    public static BigDecimal decode(String field, int scale) {
        if (field == null || field.isEmpty()) {
            throw new NumericDataException("Empty zoned decimal field");
        }
        int last = field.length() - 1;
        char sign = field.charAt(last);
        boolean negative;
        char digit;
        if (sign >= '0' && sign <= '9') {
            negative = false;
            digit = sign;
        } else if (sign == '{') {
            negative = false;
            digit = '0';
        } else if (sign >= 'A' && sign <= 'I') {
            negative = false;
            digit = (char) ('1' + (sign - 'A'));
        } else if (sign == '}') {
            negative = true;
            digit = '0';
        } else if (sign >= 'J' && sign <= 'R') {
            negative = true;
            digit = (char) ('1' + (sign - 'J'));
        } else {
            throw new NumericDataException("Invalid zoned sign '" + sign + "' in '" + field + "'");
        }
        for (int i = 0; i < last; i++) {
            char c = field.charAt(i);
            if (c < '0' || c > '9') {
                throw new NumericDataException("Non-numeric zoned decimal '" + field + "'");
            }
        }
        BigDecimal value = new BigDecimal(new BigInteger(field.substring(0, last) + digit), scale);
        return negative ? value.negate() : value;
    }

    /**
     * Encodes into {@code integerDigits + scale} characters. Signed fields overpunch the sign on the
     * last digit (positive with {@code '{'}/{@code 'A'}-{@code 'I'}, as COBOL writes zone C); unsigned
     * fields are plain digits. The value must already fit the picture (use {@link CobolDecimal#store}
     * first to reproduce a COBOL truncating MOVE); a value that does not fit is a programming error.
     */
    public static String encode(BigDecimal value, int integerDigits, int scale, boolean signed) {
        if (!CobolDecimal.fits(value, integerDigits, scale)) {
            throw new IllegalArgumentException(value + " does not fit PIC " + (signed ? "S" : "")
                    + "9(" + integerDigits + ")V9(" + scale + ")");
        }
        if (!signed && value.signum() < 0) {
            throw new IllegalArgumentException("Negative value " + value + " for an unsigned field");
        }
        BigDecimal scaled = value.setScale(scale);
        String digits = scaled.unscaledValue().abs().toString();
        int width = integerDigits + scale;
        digits = "0".repeat(width - digits.length()) + digits;
        if (!signed) {
            return digits;
        }
        int lastDigit = digits.charAt(width - 1) - '0';
        char sign;
        if (scaled.signum() < 0) {
            sign = lastDigit == 0 ? '}' : (char) ('J' + lastDigit - 1);
        } else {
            sign = lastDigit == 0 ? '{' : (char) ('A' + lastDigit - 1);
        }
        return digits.substring(0, width - 1) + sign;
    }
}
