package com.carddemo.common.cobol;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;

class CobolDecimalTest {

    @ParameterizedTest
    @CsvSource({
            "1.239, 1.23, 1.24",
            "-1.239, -1.23, -1.24",
            "1.235, 1.23, 1.24",
            "-1.235, -1.23, -1.24",
            "1.234, 1.23, 1.23",
            "0.005, 0.00, 0.01",
    })
    void truncatesUnlessRounded(String value, String truncated, String rounded) {
        assertThat(CobolDecimal.truncate(new BigDecimal(value), 2)).isEqualTo(new BigDecimal(truncated));
        assertThat(CobolDecimal.rounded(new BigDecimal(value), 2)).isEqualTo(new BigDecimal(rounded));
    }

    @Test
    void divisionTruncates() {
        assertThat(CobolDecimal.divide(new BigDecimal("10.00"), new BigDecimal("3"), 2)).isEqualTo(new BigDecimal("3.33"));
        assertThat(CobolDecimal.divide(new BigDecimal("-2.00"), new BigDecimal("3"), 2)).isEqualTo(new BigDecimal("-0.66"));
    }

    @Test
    void storeDropsHighOrderDigitsWithoutSizeError() {
        // WS-TEMP-BAL PIC S9(09)V99: 1,000,000,000.50 keeps 000,000,000.50 (D-014).
        assertThat(CobolDecimal.store(new BigDecimal("1000000000.50"), 9, 2)).isEqualTo(new BigDecimal("0.50"));
        assertThat(CobolDecimal.store(new BigDecimal("-1234567890.129"), 9, 2)).isEqualTo(new BigDecimal("-234567890.12"));
        assertThat(CobolDecimal.storeRounded(new BigDecimal("12.345"), 9, 2)).isEqualTo(new BigDecimal("12.35"));
        assertThat(CobolDecimal.storeUnsigned(new BigDecimal("-5.00"), 4, 2)).isEqualTo(new BigDecimal("5.00"));
    }

    @Test
    void fitsChecksDigitsOnBothSides() {
        assertThat(CobolDecimal.fits(new BigDecimal("999999999.99"), 9, 2)).isTrue();
        assertThat(CobolDecimal.fits(new BigDecimal("1000000000.00"), 9, 2)).isFalse();
        assertThat(CobolDecimal.fits(new BigDecimal("1.001"), 9, 2)).isFalse();
        assertThat(CobolDecimal.fits(new BigDecimal("1.100"), 9, 2)).isTrue();
        assertThat(CobolDecimal.fits(BigDecimal.ZERO, 0, 2)).isTrue();
    }
}
