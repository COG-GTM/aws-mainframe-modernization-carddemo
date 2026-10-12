package com.carddemo.interest.domain;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;

/** CBACT04C 1300-COMPUTE-INTEREST: WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200 (line 388-389). */
class InterestCalculatorTest {

    @Test
    void monthlyInterestIsBalanceTimesAnnualRateOver1200() {
        assertThat(InterestCalculator.monthlyInterest(new BigDecimal("1200.00"), new BigDecimal("12.00")))
                .isEqualByComparingTo("12.00");
    }

    @Test
    void resultIsTruncatedToTwoDecimalsNotRounded() {
        // 1000.00 * 19.99 / 1200 = 16.658333.. -> 16.65 (COMPUTE without ROUNDED)
        assertThat(InterestCalculator.monthlyInterest(new BigDecimal("1000.00"), new BigDecimal("19.99")))
                .isEqualByComparingTo("16.65");
    }

    @Test
    void negativeBalanceYieldsNegativeInterest() {
        assertThat(InterestCalculator.monthlyInterest(new BigDecimal("-100.00"), new BigDecimal("12.00")))
                .isEqualByComparingTo("-1.00");
    }

    @Test
    void zeroRateYieldsZero() {
        assertThat(InterestCalculator.monthlyInterest(new BigDecimal("999999999.99"), BigDecimal.ZERO)).isZero();
    }

    @Test
    void resultLargerThanS9_09_isHighOrderTruncatedLikePicS9_09_V99() {
        // 999,999,999.99 * 9999.99 / 1200 = 8,333,325,000 -> only the low 9 integer digits survive
        BigDecimal r = InterestCalculator.monthlyInterest(new BigDecimal("999999999.99"), new BigDecimal("9999.99"));
        assertThat(r.abs()).isLessThan(new BigDecimal("1000000000"));
    }
}
