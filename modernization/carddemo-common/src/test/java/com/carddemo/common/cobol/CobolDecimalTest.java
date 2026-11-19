package com.carddemo.common.cobol;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;

class CobolDecimalTest {

    @Test
    void valuesThatFitAreUnchangedWithTheFieldScale() {
        assertThat(CobolDecimal.store(new BigDecimal("-123.4"), 9, 2)).isEqualTo(new BigDecimal("-123.40"));
        assertThat(CobolDecimal.store(new BigDecimal("999999999.99"), 9, 2)).isEqualTo(new BigDecimal("999999999.99"));
    }

    @Test
    void highOrderDigitsAreDroppedAndTheSignKeptLikeComputeWithoutSizeError() {
        assertThat(CobolDecimal.store(new BigDecimal("1000000000.00"), 9, 2)).isEqualTo(new BigDecimal("0.00"));
        assertThat(CobolDecimal.store(new BigDecimal("1234567890.12"), 9, 2)).isEqualTo(new BigDecimal("234567890.12"));
        assertThat(CobolDecimal.store(new BigDecimal("-1234567890.12"), 9, 2)).isEqualTo(new BigDecimal("-234567890.12"));
    }

    @Test
    void excessDecimalsAreTruncatedNotRounded() {
        assertThat(CobolDecimal.store(new BigDecimal("1.239"), 9, 2)).isEqualTo(new BigDecimal("1.23"));
        assertThat(CobolDecimal.store(new BigDecimal("-1.239"), 9, 2)).isEqualTo(new BigDecimal("-1.23"));
    }

    /**
     * Expected signs observed with GnuCOBOL 3.1 ({@code .migration/tools/negative_zero_probe.cbl}, runbook R-22):
     * {@code ADD -0 TO +0} → {@code {}, {@code ADD -0 TO -0} → {@code {}, {@code 5 + -5} → {@code {},
     * {@code -999999999.99 + -0.01} into S9(09)V99 → {@code }}, {@code 999999999.99 + 0.01} → {@code {},
     * {@code -9999999999.99 + -0.01} into S9(10)V99 → {@code }}.
     */
    @Test
    void storeWithSignKeepsTheSignOfATruncatedNegativeResultOnly() {
        assertThat(CobolDecimal.storeWithSign(new BigDecimal("-1000000000.00"), 9, 2))
                .isEqualTo(new CobolDecimal.Stored(new BigDecimal("0.00"), true));
        assertThat(CobolDecimal.storeWithSign(new BigDecimal("-10000000000.00"), 10, 2))
                .isEqualTo(new CobolDecimal.Stored(new BigDecimal("0.00"), true));
        assertThat(CobolDecimal.storeWithSign(new BigDecimal("1000000000.00"), 9, 2).negativeZero()).isFalse();
        assertThat(CobolDecimal.storeWithSign(new BigDecimal("0.00"), 9, 2).negativeZero()).as("0 + -0, 5 + -5").isFalse();
        assertThat(CobolDecimal.storeWithSign(new BigDecimal("-1000000000.50"), 9, 2))
                .isEqualTo(new CobolDecimal.Stored(new BigDecimal("-0.50"), false));
        assertThat(CobolDecimal.storeWithSign(new BigDecimal("-0.01"), 9, 2))
                .isEqualTo(new CobolDecimal.Stored(new BigDecimal("-0.01"), false));
    }
}
