package com.carddemo.programs.coactupc.support;

import java.math.BigDecimal;
import java.math.RoundingMode;

/** Numeric-edited picture {@code +ZZZ,ZZZ,ZZZ.99} (WS-EDIT-CURRENCY-9-2-F). */
public final class EditedPicture {

    private EditedPicture() {
    }

    /**
     * MOVE numeric TO {@code PIC +ZZZ,ZZZ,ZZZ.99}: fixed leading sign, nine integer positions with
     * zero suppression (suppressed commas become spaces), high-order digits beyond nine are lost,
     * decimals beyond two are truncated.
     */
    public static String plusZzzCommaZzzCommaZzzDot99(BigDecimal value) {
        BigDecimal v = value.setScale(2, RoundingMode.DOWN);
        char sign = v.signum() < 0 ? '-' : '+';
        String unscaled = v.abs().unscaledValue().toString();
        unscaled = unscaled.length() < 11 ? "0".repeat(11 - unscaled.length()) + unscaled
                : unscaled.substring(unscaled.length() - 11);
        String intDigits = unscaled.substring(0, 9);
        String fraction = unscaled.substring(9);
        String pattern = "ZZZ,ZZZ,ZZZ";
        StringBuilder out = new StringBuilder();
        out.append(sign);
        boolean significant = false;
        int d = 0;
        for (int i = 0; i < pattern.length(); i++) {
            char p = pattern.charAt(i);
            if (p == ',') {
                out.append(significant ? ',' : ' ');
            } else {
                char digit = intDigits.charAt(d++);
                if (digit != '0') {
                    significant = true;
                }
                out.append(significant ? digit : ' ');
            }
        }
        out.append('.').append(fraction);
        return out.toString();
    }
}
