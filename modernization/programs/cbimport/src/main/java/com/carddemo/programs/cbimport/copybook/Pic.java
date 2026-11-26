package com.carddemo.programs.cbimport.copybook;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * COBOL MOVE and USAGE DISPLAY semantics for the PIC clauses used by the CBIMPORT copybooks.
 *
 * <p>Numeric MOVEs truncate high-order integer digits and low-order decimal digits (no rounding,
 * no ON SIZE ERROR). Unsigned receivers take the absolute value. Signed DISPLAY fields carry the
 * sign as an overpunch on the last digit ('{', 'A'-'I' positive, '}', 'J'-'R' negative), which is
 * the character form of EBCDIC zoned decimal (C0-C9 / D0-D9) and matches app/data/ASCII.
 */
public final class Pic {

    private static final String POSITIVE_OVERPUNCH = "{ABCDEFGHI";
    private static final String NEGATIVE_OVERPUNCH = "}JKLMNOPQR";
    private static final String MF_NEGATIVE_OVERPUNCH = "pqrstuvwxy";

    private Pic() {
    }

    /** MOVE to PIC X(len): left-justify, pad with spaces, truncate on the right. */
    public static String alnum(String value, int len) {
        String v = value == null ? "" : value;
        if (v.length() >= len) {
            return v.substring(0, len);
        }
        return v + " ".repeat(len - v.length());
    }

    /** MOVE to PIC 9(digits): absolute value, high-order digits truncated. */
    public static long unsigned(BigInteger value, int digits) {
        return value.abs().mod(BigInteger.TEN.pow(digits)).longValueExact();
    }

    public static long unsigned(long value, int digits) {
        return unsigned(BigInteger.valueOf(value), digits);
    }

    /** MOVE to PIC 9(intDigits)V9(scale): absolute value, both ends truncated. */
    public static BigDecimal unsignedDecimal(BigDecimal value, int intDigits, int scale) {
        return signed(value.abs(), intDigits, scale);
    }

    /** MOVE to PIC S9(intDigits)V9(scale): decimals truncated, high-order integer digits truncated. */
    public static BigDecimal signed(BigDecimal value, int intDigits, int scale) {
        BigInteger unscaled = value.setScale(scale, RoundingMode.DOWN).unscaledValue();
        BigInteger modulus = BigInteger.TEN.pow(intDigits + scale);
        BigInteger kept = unscaled.abs().mod(modulus);
        return new BigDecimal(unscaled.signum() < 0 ? kept.negate() : kept, scale);
    }

    /** USAGE DISPLAY image of PIC 9(digits). */
    public static String displayUnsigned(long value, int digits) {
        long v = unsigned(value, digits);
        String s = Long.toString(v);
        return "0".repeat(digits - s.length()) + s;
    }

    /** USAGE DISPLAY image of PIC S9(intDigits)V9(scale) with trailing overpunched sign. */
    public static String displaySigned(BigDecimal value, int intDigits, int scale) {
        BigDecimal v = signed(value, intDigits, scale);
        int total = intDigits + scale;
        String digits = v.unscaledValue().abs().toString();
        digits = "0".repeat(total - digits.length()) + digits;
        int last = digits.charAt(total - 1) - '0';
        char sign = v.signum() < 0 ? NEGATIVE_OVERPUNCH.charAt(last) : POSITIVE_OVERPUNCH.charAt(last);
        return digits.substring(0, total - 1) + sign;
    }

    /** Value of a USAGE DISPLAY PIC 9(n) image (zone portion of each byte ignored). */
    public static BigInteger parseUnsigned(String image) {
        BigInteger v = BigInteger.ZERO;
        for (int i = 0; i < image.length(); i++) {
            v = v.multiply(BigInteger.TEN).add(BigInteger.valueOf(digitOf(image.charAt(i))));
        }
        return v;
    }

    /** Value of a USAGE DISPLAY PIC S9(n)V9(scale) image with trailing overpunched sign. */
    public static BigDecimal parseSigned(String image, int scale) {
        BigInteger magnitude = parseUnsigned(image);
        char last = image.charAt(image.length() - 1);
        boolean negative = NEGATIVE_OVERPUNCH.indexOf(last) >= 0 || MF_NEGATIVE_OVERPUNCH.indexOf(last) >= 0;
        return new BigDecimal(negative ? magnitude.negate() : magnitude, scale);
    }

    private static int digitOf(char c) {
        if (c >= '0' && c <= '9') {
            return c - '0';
        }
        int idx = POSITIVE_OVERPUNCH.indexOf(c);
        if (idx < 0) {
            idx = NEGATIVE_OVERPUNCH.indexOf(c);
        }
        if (idx < 0) {
            idx = MF_NEGATIVE_OVERPUNCH.indexOf(c);
        }
        if (idx >= 0) {
            return idx;
        }
        return (c & 0x0F) % 10;
    }
}
