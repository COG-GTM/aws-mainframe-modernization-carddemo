package com.carddemo.programs.cbtrn01c.copybook;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * COBOL PICTURE semantics for USAGE DISPLAY fields: MOVE truncation rules and the
 * fixed-width (zoned decimal, trailing overpunch sign) external representation.
 */
public final class Pic {

    private static final String POSITIVE_OVERPUNCH = "{ABCDEFGHI";
    private static final String NEGATIVE_OVERPUNCH = "}JKLMNOPQR";

    private Pic() {
    }

    /** MOVE to PIC X(n): left-justified, space-padded, truncated on the right. */
    public static String alpha(String value, int length) {
        String v = value == null ? "" : value;
        if (v.length() >= length) {
            return v.substring(0, length);
        }
        return v + " ".repeat(length - v.length());
    }

    /** MOVE to PIC 9(n): sign dropped, high-order digits truncated. */
    public static long unsigned(long value, int digits) {
        return Math.floorMod(Math.abs(value), pow10(digits));
    }

    /** MOVE to PIC S9(i)V9(s): low-order decimals truncated (no rounding), high-order digits truncated. */
    public static BigDecimal signed(BigDecimal value, int intDigits, int scale) {
        BigDecimal v = value == null ? BigDecimal.ZERO : value;
        BigDecimal scaled = v.setScale(scale, RoundingMode.DOWN);
        BigInteger unscaled = scaled.unscaledValue();
        BigInteger modulus = BigInteger.TEN.pow(intDigits + scale);
        BigInteger magnitude = unscaled.abs().mod(modulus);
        BigInteger result = unscaled.signum() < 0 ? magnitude.negate() : magnitude;
        return new BigDecimal(result, scale);
    }

    public static String formatUnsigned(long value, int digits) {
        String s = Long.toString(unsigned(value, digits));
        return "0".repeat(digits - s.length()) + s;
    }

    /** Zoned decimal with the sign overpunched on the last digit ({,A-I positive; },J-R negative). */
    public static String formatSigned(BigDecimal value, int intDigits, int scale) {
        BigDecimal v = signed(value, intDigits, scale);
        String digits = v.unscaledValue().abs().toString();
        int total = intDigits + scale;
        digits = "0".repeat(total - digits.length()) + digits;
        int last = digits.charAt(total - 1) - '0';
        char sign = v.signum() < 0 ? NEGATIVE_OVERPUNCH.charAt(last) : POSITIVE_OVERPUNCH.charAt(last);
        return digits.substring(0, total - 1) + sign;
    }

    public static long parseUnsigned(String text) {
        for (int i = 0; i < text.length(); i++) {
            if (!isDigit(text.charAt(i))) {
                throw new IllegalArgumentException("Non-numeric data in PIC 9(" + text.length() + "): '" + text + "'");
            }
        }
        return Long.parseLong(text);
    }

    public static BigDecimal parseSigned(String text, int scale) {
        int n = text.length();
        String head = text.substring(0, n - 1);
        char signChar = text.charAt(n - 1);
        int lastDigit;
        boolean negative = false;
        if (isDigit(signChar)) {
            lastDigit = signChar - '0';
        } else if (POSITIVE_OVERPUNCH.indexOf(signChar) >= 0) {
            lastDigit = POSITIVE_OVERPUNCH.indexOf(signChar);
        } else if (NEGATIVE_OVERPUNCH.indexOf(signChar) >= 0) {
            lastDigit = NEGATIVE_OVERPUNCH.indexOf(signChar);
            negative = true;
        } else {
            throw new IllegalArgumentException("Invalid sign/digit in signed numeric: '" + text + "'");
        }
        for (int i = 0; i < head.length(); i++) {
            if (!isDigit(head.charAt(i))) {
                throw new IllegalArgumentException("Non-numeric data in signed numeric: '" + text + "'");
            }
        }
        BigInteger unscaled = new BigInteger(head + lastDigit);
        return new BigDecimal(negative ? unscaled.negate() : unscaled, scale);
    }

    private static boolean isDigit(char c) {
        return c >= '0' && c <= '9';
    }

    private static long pow10(int digits) {
        long r = 1;
        for (int i = 0; i < digits; i++) {
            r *= 10;
        }
        return r;
    }

    /** Sequential cursor over a fixed-width record image. */
    public static final class Reader {
        private final String image;
        private int pos;

        public Reader(String image, int recordLength) {
            if (image.length() > recordLength) {
                throw new IllegalArgumentException(
                        "Record longer than " + recordLength + " bytes: " + image.length());
            }
            this.image = alpha(image, recordLength);
        }

        public String x(int len) {
            String s = image.substring(pos, pos + len);
            pos += len;
            return s;
        }

        public long n(int digits) {
            return parseUnsigned(x(digits));
        }

        public BigDecimal s(int intDigits, int scale) {
            return parseSigned(x(intDigits + scale), scale);
        }
    }
}
