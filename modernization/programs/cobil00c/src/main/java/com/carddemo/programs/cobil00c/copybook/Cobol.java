package com.carddemo.programs.cobil00c.copybook;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/** Helpers reproducing COBOL MOVE / figurative-constant semantics on fixed-width fields. */
public final class Cobol {

    public static final char LOW_VALUE = '\u0000';
    public static final char HIGH_VALUE = '\u00FF';

    private Cobol() {
    }

    /** MOVE alphanumeric: left-justify, pad with spaces, truncate on the right. */
    public static String alnum(String value, int length) {
        String v = value == null ? "" : value;
        if (v.length() >= length) {
            return v.substring(0, length);
        }
        return v + " ".repeat(length - v.length());
    }

    public static String spaces(int length) {
        return " ".repeat(length);
    }

    public static String lowValues(int length) {
        return String.valueOf(LOW_VALUE).repeat(length);
    }

    public static String highValues(int length) {
        return String.valueOf(HIGH_VALUE).repeat(length);
    }

    /** Figurative comparison {@code field = SPACES}: every byte is a space. */
    public static boolean isSpaces(String value) {
        return value != null && value.chars().allMatch(c -> c == ' ');
    }

    /** Figurative comparison {@code field = LOW-VALUES}: every byte is X'00'. */
    public static boolean isLowValues(String value) {
        return value != null && value.chars().allMatch(c -> c == LOW_VALUE);
    }

    public static boolean isSpacesOrLowValues(String value) {
        return isSpaces(value) || isLowValues(value);
    }

    /** MOVE numeric to PIC 9(n): high-order truncation, unsigned, zero-filled. */
    public static String unsignedDigits(long value, int digits) {
        BigInteger mod = BigInteger.TEN.pow(digits);
        BigInteger v = BigInteger.valueOf(value).abs().mod(mod);
        String s = v.toString();
        return "0".repeat(digits - s.length()) + s;
    }

    /**
     * MOVE alphanumeric to PIC 9(n) DISPLAY then use it in arithmetic: each byte contributes its
     * low-order nibble (the zoned-decimal digit). A digit nibble above 9 (e.g. HIGH-VALUES) is a
     * data exception (S0C7) on the mainframe and is reported here as {@link NumberFormatException}.
     */
    public static long zonedValue(String raw) {
        long result = 0;
        for (int i = 0; i < raw.length(); i++) {
            int nibble = raw.charAt(i) & 0x0F;
            if (nibble > 9) {
                throw new NumberFormatException("invalid zoned digit X'" + Integer.toHexString(raw.charAt(i))
                        + "' at offset " + i);
            }
            result = result * 10 + nibble;
        }
        return result;
    }

    /** MOVE to PIC S9(int)V9(scale): decimal alignment, truncation of excess digits on both ends. */
    public static BigDecimal toPic(BigDecimal value, int integerDigits, int scale) {
        BigDecimal scaled = value.setScale(scale, RoundingMode.DOWN);
        BigInteger unscaled = scaled.unscaledValue();
        BigInteger mod = BigInteger.TEN.pow(integerDigits + scale);
        BigInteger truncated = unscaled.abs().mod(mod);
        if (unscaled.signum() < 0) {
            truncated = truncated.negate();
        }
        return new BigDecimal(truncated, scale);
    }

    /**
     * MOVE S9(n)V99 to an edited picture {@code +9(int).99}: leading sign ('+' or '-'),
     * zero-filled integer part (high-order truncated), period, two decimals.
     */
    public static String editSignedFixed(BigDecimal value, int integerDigits) {
        BigDecimal v = toPic(value, integerDigits, 2);
        String sign = v.signum() < 0 ? "-" : "+";
        String digits = v.abs().unscaledValue().toString();
        int total = integerDigits + 2;
        digits = "0".repeat(Math.max(0, total - digits.length())) + digits;
        return sign + digits.substring(0, integerDigits) + "." + digits.substring(integerDigits);
    }

    /**
     * DISPLAY of a signed numeric (e.g. {@code PIC S9(09) COMP}): zoned decimal with the sign
     * overpunched in the last digit, as Enterprise COBOL prints it.
     */
    public static String displaySigned(long value, int digits) {
        return ZonedDecimal.encode(BigDecimal.valueOf(value), digits, 0);
    }
}
