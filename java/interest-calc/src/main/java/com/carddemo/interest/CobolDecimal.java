package com.carddemo.interest;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * Fixed-point semantics of COBOL {@code PIC S9(n)V99} fields.
 *
 * <p>A COMPUTE without the ROUNDED phrase truncates the intermediate result to the scale of the
 * receiving field, and a value whose integer part exceeds the receiving PICTURE is stored with its
 * high-order digits dropped (no SIZE ERROR clause is coded in CBACT04C).
 */
public final class CobolDecimal {

    private CobolDecimal() {
    }

    /** Scale of every monetary field in the copybooks used by CBACT04C (V99). */
    public static final int MONEY_SCALE = 2;

    /** Integer digits of {@code WS-MONTHLY-INT}, {@code WS-TOTAL-INT} and {@code TRAN-AMT}: S9(09)V99. */
    public static final int AMOUNT_DIGITS = 9;

    /** Integer digits of {@code ACCT-CURR-BAL}: S9(10)V99. */
    public static final int BALANCE_DIGITS = 10;

    public static final BigDecimal ZERO_MONEY = BigDecimal.ZERO.setScale(MONEY_SCALE);

    /**
     * Truncates {@code value} toward zero to two decimals and drops high-order digits that do not
     * fit in {@code integerDigits}, reproducing a MOVE into a {@code PIC S9(integerDigits)V99} field.
     */
    public static BigDecimal toPicture(BigDecimal value, int integerDigits) {
        BigDecimal truncated = value.setScale(MONEY_SCALE, RoundingMode.DOWN);
        BigDecimal modulus = BigDecimal.TEN.pow(integerDigits);
        BigDecimal integerPart = truncated.abs().setScale(0, RoundingMode.DOWN);
        if (integerPart.compareTo(modulus) < 0) {
            return truncated;
        }
        BigDecimal wrapped = truncated.abs().remainder(modulus).setScale(MONEY_SCALE, RoundingMode.DOWN);
        return truncated.signum() < 0 ? wrapped.negate() : wrapped;
    }

    /** {@link #toPicture} for the S9(09)V99 amount fields. */
    public static BigDecimal toAmount(BigDecimal value) {
        return toPicture(value, AMOUNT_DIGITS);
    }

    /** {@link #toPicture} for the S9(10)V99 account balance fields. */
    public static BigDecimal toBalance(BigDecimal value) {
        return toPicture(value, BALANCE_DIGITS);
    }

    /** Left-pads {@code value} with zeroes to {@code length}, as a MOVE into a {@code PIC 9(length)} field. */
    public static String zoned(long value, int length) {
        return String.format("%0" + length + "d", value);
    }

    /** Left-pads or right-truncates {@code value} to {@code length} digits, as a {@code PIC 9(length)} field. */
    public static String zoned(String value, int length) {
        String source = value == null ? "" : value.trim();
        if (source.isEmpty()) {
            source = "0";
        }
        if (source.length() >= length) {
            return source.substring(source.length() - length);
        }
        return "0".repeat(length - source.length()) + source;
    }

    /** Positive and negative trailing sign overpunch characters of zoned decimal, indexed by digit. */
    private static final String POSITIVE_OVERPUNCH = "{ABCDEFGHI";
    private static final String NEGATIVE_OVERPUNCH = "}JKLMNOPQR";

    /**
     * Renders {@code value} as an unpacked {@code PIC S9(integerDigits)V99} DISPLAY field: the
     * implied decimal point is not written and the sign is overpunched on the trailing digit.
     */
    public static String signedZoned(BigDecimal value, int integerDigits) {
        BigDecimal fitted = toPicture(value, integerDigits);
        String digits = zoned(fitted.abs().movePointRight(MONEY_SCALE).toBigInteger().toString(),
                integerDigits + MONEY_SCALE);
        int lastDigit = digits.charAt(digits.length() - 1) - '0';
        String overpunch = fitted.signum() < 0 ? NEGATIVE_OVERPUNCH : POSITIVE_OVERPUNCH;
        return digits.substring(0, digits.length() - 1) + overpunch.charAt(lastDigit);
    }

    /** Pads or truncates {@code value} to {@code length}, as a MOVE into a {@code PIC X(length)} field. */
    public static String alphanumeric(String value, int length) {
        String source = value == null ? "" : value;
        if (source.length() >= length) {
            return source.substring(0, length);
        }
        return source + " ".repeat(length - source.length());
    }
}
