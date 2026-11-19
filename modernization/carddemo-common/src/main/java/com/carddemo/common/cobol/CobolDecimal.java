package com.carddemo.common.cobol;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * Arithmetic results stored the way COBOL stores them in a {@code PIC S9(i)V9(d)} receiving field when the statement has
 * neither {@code ROUNDED} nor {@code ON SIZE ERROR}: excess decimals are truncated and excess high-order integer digits are
 * dropped (the sign is kept).
 */
public final class CobolDecimal {

    private CobolDecimal() {
    }

    public static BigDecimal store(BigDecimal value, int integerDigits, int scale) {
        BigDecimal truncated = value.setScale(scale, RoundingMode.DOWN);
        return truncated.remainder(BigDecimal.TEN.pow(integerDigits)).setScale(scale, RoundingMode.UNNECESSARY);
    }
}
