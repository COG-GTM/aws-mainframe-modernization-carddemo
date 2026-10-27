package com.carddemo.transaction.cobol;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * USAGE DISPLAY signed numerics (e.g. {@code PIC S9(09)V99}) with the sign over-punched in the last
 * byte (SIGN TRAILING, not SEPARATE), as found in the CardDemo ASCII extracts.
 * <pre>
 *   positive: '{'=0 'A'=1 ... 'I'=9   (also plain '0'..'9' = unsigned/positive)
 *   negative: '}'=0 'J'=1 ... 'R'=9   (also GnuCOBOL ASCII zones 'p'..'y')
 * </pre>
 */
public final class ZonedDecimal {

    private static final String POSITIVE = "{ABCDEFGHI";
    private static final String NEGATIVE = "}JKLMNOPQR";
    private static final String GNU_NEGATIVE = "pqrstuvwxy";

    private ZonedDecimal() {
    }

    public static BigDecimal decode(String field, int scale) {
        if (field == null || field.isEmpty()) {
            throw new CobolDataException("empty zoned decimal field");
        }
        int last = field.length() - 1;
        char signChar = field.charAt(last);
        int lastDigit;
        boolean negative = false;
        if (signChar >= '0' && signChar <= '9') {
            lastDigit = signChar - '0';
        } else if (POSITIVE.indexOf(signChar) >= 0) {
            lastDigit = POSITIVE.indexOf(signChar);
        } else if (NEGATIVE.indexOf(signChar) >= 0) {
            lastDigit = NEGATIVE.indexOf(signChar);
            negative = true;
        } else if (GNU_NEGATIVE.indexOf(signChar) >= 0) {
            lastDigit = GNU_NEGATIVE.indexOf(signChar);
            negative = true;
        } else {
            throw new CobolDataException("invalid sign/digit '" + signChar + "' in zoned decimal '" + field + "'");
        }
        String leading = field.substring(0, last);
        if (!leading.isEmpty() && !Pic.isNumeric(leading)) {
            throw new CobolDataException("non-numeric data in zoned decimal '" + field + "'");
        }
        BigInteger unscaled = new BigInteger(leading + lastDigit);
        if (negative) {
            unscaled = unscaled.negate();
        }
        return new BigDecimal(unscaled, scale);
    }

    /** Unsigned {@code PIC 9(n)} DISPLAY field. */
    public static long decodeUnsigned(String field) {
        if (!Pic.isNumeric(field)) {
            throw new CobolDataException("non-numeric data in unsigned numeric field '" + field + "'");
        }
        return Long.parseLong(field);
    }

    public static String encode(BigDecimal value, int integerDigits, int scale) {
        BigDecimal v = Pic.s9(value, integerDigits, scale);
        String digits = v.unscaledValue().abs().toString();
        int width = integerDigits + scale;
        digits = "0".repeat(width - digits.length()) + digits;
        int lastDigit = digits.charAt(width - 1) - '0';
        char sign = v.signum() < 0 ? NEGATIVE.charAt(lastDigit) : POSITIVE.charAt(lastDigit);
        return digits.substring(0, width - 1) + sign;
    }
}
