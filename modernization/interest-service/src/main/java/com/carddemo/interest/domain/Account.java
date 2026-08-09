package com.carddemo.interest.domain;

import com.carddemo.mainframe.io.CobolFixedPoint;

import java.math.BigDecimal;

/**
 * The account master record ({@code CVACT01Y}, {@code app/cpy/CVACT01Y.cpy:4-17}).
 *
 * <p>Immutable: the interest cycle produces a new {@code Account} through
 * {@link #withInterestPosted(BigDecimal)} instead of mutating a shared record area, which is what
 * the COBOL {@code REWRITE ... FROM ACCOUNT-RECORD} does in place.
 *
 * <p>Monetary fields are {@code PIC S9(10)V99}, so they are modelled as {@link BigDecimal} with
 * scale 2 and a ten-digit integral capacity.
 */
public record Account(AccountId id,
                      String activeStatus,
                      BigDecimal currentBalance,
                      BigDecimal creditLimit,
                      BigDecimal cashCreditLimit,
                      String openDate,
                      String expirationDate,
                      String reissueDate,
                      BigDecimal currentCycleCredit,
                      BigDecimal currentCycleDebit,
                      String addressZip,
                      String groupId) {

    /** Integer digits of {@code PIC S9(10)V99} money fields on the account record. */
    private static final int MONEY_INTEGER_DIGITS = 10;
    private static final int MONEY_SCALE = 2;

    /**
     * Applies the monthly interest cycle to the account: adds the accrued interest to the current
     * balance and clears both cycle-to-date buckets.
     *
     * <p>Business rule BR-6, COBOL paragraph {@code 1050-UPDATE-ACCOUNT}
     * ({@code app/cbl/CBACT04C.cbl:350-354}):
     * {@code ADD WS-TOTAL-INT TO ACCT-CURR-BAL}, {@code MOVE 0 TO ACCT-CURR-CYC-CREDIT} and
     * {@code MOVE 0 TO ACCT-CURR-CYC-DEBIT}. The addition truncates into the receiving
     * {@code PIC S9(10)V99} field, which {@link CobolFixedPoint#fit} reproduces.
     */
    public Account withInterestPosted(BigDecimal totalInterest) {
        BigDecimal newBalance = CobolFixedPoint.fit(
                currentBalance.add(totalInterest), MONEY_INTEGER_DIGITS, MONEY_SCALE);
        return new Account(id, activeStatus, newBalance, creditLimit, cashCreditLimit,
                openDate, expirationDate, reissueDate,
                BigDecimal.ZERO.setScale(MONEY_SCALE), BigDecimal.ZERO.setScale(MONEY_SCALE),
                addressZip, groupId);
    }

    /**
     * The pricing group used to look up disclosed interest rates, as moved into the disclosure
     * group key at {@code app/cbl/CBACT04C.cbl:210}. Blank on every account in the static export,
     * which is what drives the {@code DEFAULT} fallback.
     */
    public String pricingGroupId() {
        return groupId == null ? "" : groupId.trim();
    }
}
