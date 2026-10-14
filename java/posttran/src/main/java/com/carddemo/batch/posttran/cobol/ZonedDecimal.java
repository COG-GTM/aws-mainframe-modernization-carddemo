package com.carddemo.batch.posttran.cobol;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * DISPLAY (zoned) numerics such as {@code PIC S9(09)V99}: one character per digit, the sign
 * overpunched on the last digit. Positive is {@code {ABCDEFGHI}} (or a plain digit), negative is
 * {@code }JKLMNOPQR}; this is the CP037 rendering of the X'Cn'/X'Dn' zone nibbles. The Micro Focus
 * ASCII convention ({@code p}..{@code y} = negative) is accepted on input.
 */
public final class ZonedDecimal {

    private static final String POSITIVE = "{ABCDEFGHI";
    private static final String NEGATIVE = "}JKLMNOPQR";
    private static final String MF_NEGATIVE = "pqrstuvwxy";

    private ZonedDecimal() {
    }

    public static BigDecimal decodeSigned(String field, int scale) {
        int last = field.length() - 1;
        char sign = field.charAt(last);
        int digit;
        boolean negative = false;
        if (sign >= '0' && sign <= '9') {
            digit = sign - '0';
        } else if (POSITIVE.indexOf(sign) >= 0) {
            digit = POSITIVE.indexOf(sign);
        } else if (NEGATIVE.indexOf(sign) >= 0) {
            digit = NEGATIVE.indexOf(sign);
            negative = true;
        } else if (MF_NEGATIVE.indexOf(sign) >= 0) {
            digit = MF_NEGATIVE.indexOf(sign);
            negative = true;
        } else {
            throw new NumberFormatException("Invalid zoned sign '" + sign + "' in [" + field + "]");
        }
        String digits = digits(field.substring(0, last)) + digit;
        BigDecimal value = new BigDecimal(new BigInteger(digits), scale);
        return negative ? value.negate() : value;
    }

    public static String encodeSigned(BigDecimal value, int integerDigits, int scale) {
        BigDecimal v = truncate(value, integerDigits, scale);
        String digits = v.unscaledValue().abs().toString();
        int length = integerDigits + scale;
        String padded = "0".repeat(length - digits.length()) + digits;
        int lastDigit = padded.charAt(length - 1) - '0';
        char sign = v.signum() < 0 ? NEGATIVE.charAt(lastDigit) : POSITIVE.charAt(lastDigit);
        return padded.substring(0, length - 1) + sign;
    }

    public static long decodeUnsigned(String field) {
        return Long.parseLong(digits(field));
    }

    public static String encodeUnsigned(long value, int digits) {
        String s = Long.toString(Math.abs(value));
        if (s.length() > digits) {
            s = s.substring(s.length() - digits);
        }
        return "0".repeat(digits - s.length()) + s;
    }

    /**
     * Storing into {@code S9(integerDigits)V9(scale)} without ON SIZE ERROR: excess fraction digits
     * and high-order integer digits are dropped.
     */
    public static BigDecimal truncate(BigDecimal value, int integerDigits, int scale) {
        BigDecimal scaled = value.setScale(scale, RoundingMode.DOWN);
        BigInteger modulus = BigInteger.TEN.pow(integerDigits + scale);
        BigInteger unscaled = scaled.unscaledValue();
        BigInteger kept = unscaled.abs().mod(modulus);
        return new BigDecimal(unscaled.signum() < 0 ? kept.negate() : kept, scale);
    }

    private static String digits(String s) {
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c < '0' || c > '9') {
                throw new NumberFormatException("Non-numeric data in DISPLAY numeric [" + s + "]");
            }
        }
        return s.isEmpty() ? "0" : s;
    }
}
