package com.carddemo.interest.domain;

import java.math.BigDecimal;

/**
 * A disclosed annual interest rate for one pricing group and transaction category.
 *
 * <p>Source copybook {@code CVTRA02Y} ({@code app/cpy/CVTRA02Y.cpy:4-10}). The rate is an annual
 * percentage ({@code DIS-INT-RATE PIC S9(04)V99}, e.g. {@code 15.00} meaning 15% APR), not a
 * fraction; the division by 1200 in {@code 1300-COMPUTE-INTEREST}
 * ({@code app/cbl/CBACT04C.cbl:464-465}) converts percent-per-year into a monthly fraction.
 */
public record DisclosureGroup(DisclosureGroupKey key, BigDecimal annualRatePercent) {

    /** Whether this row prices the category at zero interest, which suppresses accrual. */
    public boolean isZeroRate() {
        return annualRatePercent.signum() == 0;
    }
}
