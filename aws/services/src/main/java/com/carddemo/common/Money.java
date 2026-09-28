package com.carddemo.common;

import java.math.BigDecimal;
import java.math.RoundingMode;

public final class Money {

    private Money() {
    }

    public static String format(BigDecimal value) {
        if (value == null) {
            return null;
        }
        return value.setScale(2, RoundingMode.DOWN).toPlainString();
    }
}
