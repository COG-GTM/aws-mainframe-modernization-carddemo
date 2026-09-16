package com.carddemo.report.domain;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * The two numeric-edited pictures of CVTRA07Y: {@code -ZZZ,ZZZ,ZZZ.ZZ} (detail amount) and
 * {@code +ZZZ,ZZZ,ZZZ.ZZ} (totals). 15 characters. Floating sign is placed immediately left of
 * the first significant character; with '-' a positive value shows a space, with '+' it shows '+'.
 * A zero value with every digit suppressed prints as all spaces (IBM Enterprise COBOL rule).
 * Values are truncated, not rounded, to two decimals and to nine integer digits (MOVE semantics).
 */
public final class CobolEditedAmount {

    public static final int WIDTH = 15;

    private CobolEditedAmount() {
    }

    public static String minusEdited(BigDecimal value) {
        return edit(value, false);
    }

    public static String plusEdited(BigDecimal value) {
        return edit(value, true);
    }

    private static String edit(BigDecimal value, boolean plusPicture) {
        BigDecimal scaled = value.setScale(2, RoundingMode.DOWN);
        BigDecimal abs = scaled.abs().remainder(new BigDecimal("1000000000"));
        if (abs.signum() == 0) {
            return " ".repeat(WIDTH);
        }
        String digits = String.format("%011d", abs.unscaledValue());   // 9 integer + 2 decimal digits
        String intPart = digits.substring(0, 9);
        String frac = digits.substring(9);
        StringBuilder body = new StringBuilder();
        for (int i = 0; i < 9; i++) {
            body.append(intPart.charAt(i));
            if (i == 2 || i == 5) {
                body.append(',');
            }
        }
        // zero-suppress leading zeros and the commas inside the suppressed area
        int firstSignificant = 0;
        while (firstSignificant < body.length()
                && (body.charAt(firstSignificant) == '0' || body.charAt(firstSignificant) == ',')) {
            firstSignificant++;
        }
        String suppressed = " ".repeat(firstSignificant) + body.substring(firstSignificant);
        String unsigned = suppressed + "." + frac;               // 14 chars: 11 + '.' + 2
        char sign = scaled.signum() < 0 ? '-' : (plusPicture ? '+' : ' ');
        StringBuilder padded = new StringBuilder(" " + unsigned);  // 15 chars incl. the floating sign slot
        int firstNonSpace = 1;
        while (padded.charAt(firstNonSpace) == ' ') {
            firstNonSpace++;
        }
        padded.setCharAt(firstNonSpace - 1, sign);
        return padded.toString();
    }
}
