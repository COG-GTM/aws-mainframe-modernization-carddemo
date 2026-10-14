package com.carddemo.runtime.cobol;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * PIC 9(n)V9(s) / S9(n)V9(s) USAGE DISPLAY (zoned decimal) codec.
 *
 * <p>Signed fields use a trailing overpunched sign, encoded with the characters an EBCDIC zoned sign
 * becomes after text conversion to ASCII (the convention of the files in app/data/ASCII):
 * positive {@code {ABCDEFGHI} } for digits 0-9, negative <code>}JKLMNOPQR</code>.
 * The decoder additionally accepts plain digits (treated as positive) and the GnuCOBOL/MF ASCII
 * negative convention {@code p}-{@code y}.
 */
public final class ZonedDecimal {

    private static final String POSITIVE = "{ABCDEFGHI";
    private static final String NEGATIVE = "}JKLMNOPQR";
    private static final String ASCII_NEGATIVE = "pqrstuvwxy";

    private ZonedDecimal() {
    }

    /** Field length in bytes: one per digit (trailing sign is overpunched, not separate). */
    public static int length(int integerDigits, int scale) {
        return integerDigits + scale;
    }

    public static String encode(BigDecimal value, int integerDigits, int scale, boolean signed) {
        BigDecimal stored = CobolNumeric.store(value, integerDigits, scale, signed);
        int digits = integerDigits + scale;
        String body = stored.unscaledValue().abs().toString();
        StringBuilder sb = new StringBuilder(digits);
        for (int i = body.length(); i < digits; i++) {
            sb.append('0');
        }
        sb.append(body);
        if (signed && digits > 0) {
            int last = sb.charAt(digits - 1) - '0';
            sb.setCharAt(digits - 1, stored.signum() < 0 ? NEGATIVE.charAt(last) : POSITIVE.charAt(last));
        }
        return sb.toString();
    }

    /**
     * Decodes a zoned field. Throws {@link NumberFormatException} for non-numeric content (the condition
     * that would raise S0C7 on the mainframe); use {@link #isNumeric} to test first (IF ... NUMERIC).
     */
    public static BigDecimal decode(String text, int scale) {
        if (text.isEmpty()) {
            return BigDecimal.ZERO.setScale(scale);
        }
        StringBuilder digits = new StringBuilder(text.length());
        boolean negative = false;
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            if (c >= '0' && c <= '9') {
                digits.append(c);
            } else if (i == text.length() - 1 && POSITIVE.indexOf(c) >= 0) {
                digits.append((char) ('0' + POSITIVE.indexOf(c)));
            } else if (i == text.length() - 1 && NEGATIVE.indexOf(c) >= 0) {
                digits.append((char) ('0' + NEGATIVE.indexOf(c)));
                negative = true;
            } else if (i == text.length() - 1 && ASCII_NEGATIVE.indexOf(c) >= 0) {
                digits.append((char) ('0' + ASCII_NEGATIVE.indexOf(c)));
                negative = true;
            } else {
                throw new NumberFormatException("Non-numeric zoned decimal data: '" + text + "'");
            }
        }
        BigInteger unscaled = new BigInteger(digits.toString());
        return new BigDecimal(negative ? unscaled.negate() : unscaled, scale);
    }

    /** {@code IF field NUMERIC} for a zoned item. Unsigned fields accept only digits. */
    public static boolean isNumeric(String text, boolean signed) {
        if (text.isEmpty()) {
            return false;
        }
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            boolean digit = c >= '0' && c <= '9';
            boolean lastSign = signed && i == text.length() - 1
                    && (POSITIVE.indexOf(c) >= 0 || NEGATIVE.indexOf(c) >= 0 || ASCII_NEGATIVE.indexOf(c) >= 0);
            if (!digit && !lastSign) {
                return false;
            }
        }
        return true;
    }
}
