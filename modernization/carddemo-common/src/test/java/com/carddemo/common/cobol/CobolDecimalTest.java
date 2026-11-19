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
}
