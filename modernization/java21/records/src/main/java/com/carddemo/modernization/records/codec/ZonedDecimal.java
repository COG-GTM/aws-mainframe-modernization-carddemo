package com.carddemo.modernization.records.codec;

import java.math.BigDecimal;
import java.util.Objects;

/** DISPLAY value with the otherwise lost negative-zero sign retained. */
public record ZonedDecimal(BigDecimal value, boolean negativeZero) {
    public ZonedDecimal {
        Objects.requireNonNull(value);
        if (negativeZero && value.signum() != 0) throw new IllegalArgumentException("Negative-zero flag on nonzero value");
    }
    public ZonedDecimal(BigDecimal value) { this(value, false); }
}
