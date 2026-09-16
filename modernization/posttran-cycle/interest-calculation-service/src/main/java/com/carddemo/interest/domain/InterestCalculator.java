package com.carddemo.interest.domain;

import com.carddemo.recordio.codec.CobolNumeric;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;

/**
 * CBACT04C 1300-COMPUTE-INTEREST (app/cbl/CBACT04C.cbl lines 462-466):
 * {@code WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200} into {@code S9(09)V99} with no
 * ROUNDED phrase, i.e. the quotient is truncated toward zero at two decimals.
 */
public final class InterestCalculator {

    static final BigDecimal ANNUAL_PERCENT_TO_MONTHLY = new BigDecimal("1200");
    static final int INT_DIGITS = 9;
    static final int SCALE = 2;

    private InterestCalculator() {
    }

    public static BigDecimal monthlyInterest(BigDecimal categoryBalance, BigDecimal annualRatePercent) {
        BigDecimal quotient = categoryBalance.multiply(annualRatePercent)
                .divide(ANNUAL_PERCENT_TO_MONTHLY, MathContext.DECIMAL128)
                .setScale(SCALE, RoundingMode.DOWN);
        return CobolNumeric.truncate(quotient, INT_DIGITS, SCALE);
    }
}
