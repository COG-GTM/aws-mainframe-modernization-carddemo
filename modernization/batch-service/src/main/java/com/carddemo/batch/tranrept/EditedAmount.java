package com.carddemo.batch.tranrept;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * The two amount pictures of {@code CVTRA07Y}: {@code -ZZZ,ZZZ,ZZZ.ZZ} (TRAN-REPORT-AMT) and
 * {@code +ZZZ,ZZZ,ZZZ.ZZ} (the page, account and grand totals). Leading zeros and the commas
 * between them become spaces up to the decimal point; the fraction is always printed. The sign
 * position holds '-' for a negative value, otherwise ' ' ({@code -}) or '+' ({@code +}). A zero
 * value is all spaces, sign included, as every digit position is a Z (checked against GnuCOBOL in
 * {@code EditedAmountTest}).
 */
public final class EditedAmount {

    public static final int LENGTH = 15;

    private EditedAmount() {
    }

    /** {@code PIC -ZZZ,ZZZ,ZZZ.ZZ}. */
    public static String detail(BigDecimal amount) {
        return format(amount, ' ');
    }

    /** {@code PIC +ZZZ,ZZZ,ZZZ.ZZ}. */
    public static String total(BigDecimal amount) {
        return format(amount, '+');
    }

    private static String format(BigDecimal amount, char positiveSign) {
        long cents = amount.setScale(2, RoundingMode.DOWN).unscaledValue().longValueExact();
        if (cents == 0) {
            return " ".repeat(LENGTH);
        }
        String digits = String.format("%011d", Math.abs(cents));
        if (digits.length() > 11) {
            throw new IllegalArgumentException("Amount does not fit S9(09)V99: " + amount);
        }
        String integer = digits.substring(0, 3) + "," + digits.substring(3, 6) + "," + digits.substring(6, 9);
        StringBuilder out = new StringBuilder(LENGTH);
        out.append(cents < 0 ? '-' : positiveSign);
        boolean suppressing = true;
        for (char c : integer.toCharArray()) {
            if (suppressing && (c == '0' || c == ',')) {
                out.append(' ');
            } else {
                suppressing = false;
                out.append(c);
            }
        }
        return out.append('.').append(digits, 9, 11).toString();
    }
}
