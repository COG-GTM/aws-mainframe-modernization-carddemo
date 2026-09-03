package com.carddemo.interest.rules;

import com.carddemo.mainframe.io.CobolFixedPoint;

import java.math.BigDecimal;
import java.math.RoundingMode;

/** Converts a category balance and a disclosed annual rate into one month's interest. */
public final class MonthlyInterestCalculator {

    /** Twelve months times one hundred percent points, the divisor used by the COBOL COMPUTE. */
    private static final BigDecimal MONTHS_TIMES_PERCENT = BigDecimal.valueOf(1200);

    /** Working precision before the fixed-point store; far wider than any CardDemo value. */
    private static final int WORKING_SCALE = 20;

    /** {@code WS-MONTHLY-INT PIC S9(09)V99} ({@code app/cbl/CBACT04C.cbl:168}). */
    private static final int RESULT_INTEGER_DIGITS = 9;
    private static final int RESULT_SCALE = 2;

    /**
     * Business rule BR-5 — monthly interest for one transaction-category balance.
     *
     * <p>COBOL paragraph {@code 1300-COMPUTE-INTEREST}
     * ({@code app/cbl/CBACT04C.cbl:462-467}):
     * {@code COMPUTE WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200}. The rate is an annual
     * percentage, so dividing by 1200 turns "percent per year" into "fraction per month". The
     * {@code COMPUTE} has no {@code ROUNDED} phrase, therefore the result is truncated toward zero
     * into the two-decimal receiving field rather than rounded.
     *
     * @param categoryBalance {@code TRAN-CAT-BAL PIC S9(09)V99}
     * @param annualRatePercent {@code DIS-INT-RATE PIC S9(04)V99}
     * @return interest for the month, truncated to {@code PIC S9(09)V99}
     */
    public BigDecimal monthlyInterest(BigDecimal categoryBalance, BigDecimal annualRatePercent) {
        BigDecimal quotient = categoryBalance.multiply(annualRatePercent)
                .divide(MONTHS_TIMES_PERCENT, WORKING_SCALE, RoundingMode.DOWN);
        return CobolFixedPoint.fit(quotient, RESULT_INTEGER_DIGITS, RESULT_SCALE);
    }

    /**
     * Business rule BR-5 (continued) — accumulation of category interest into the account total.
     *
     * <p>COBOL {@code ADD WS-MONTHLY-INT TO WS-TOTAL-INT} ({@code app/cbl/CBACT04C.cbl:467}),
     * where {@code WS-TOTAL-INT} is also {@code PIC S9(09)V99}
     * ({@code app/cbl/CBACT04C.cbl:169}) and is reset to zero at every account break
     * ({@code app/cbl/CBACT04C.cbl:200}).
     */
    public BigDecimal accumulate(BigDecimal runningTotal, BigDecimal monthlyInterest) {
        return CobolFixedPoint.fit(runningTotal.add(monthlyInterest), RESULT_INTEGER_DIGITS, RESULT_SCALE);
    }
}
