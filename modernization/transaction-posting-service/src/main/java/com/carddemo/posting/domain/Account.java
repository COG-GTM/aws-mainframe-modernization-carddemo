package com.carddemo.posting.domain;

import com.carddemo.mainframe.io.CobolFixedPoint;

import java.math.BigDecimal;

/**
 * The account master record ({@code CVACT01Y}, {@code app/cpy/CVACT01Y.cpy:4-17}).
 *
 * <p>Immutable: posting a transaction produces a new {@code Account} through
 * {@link #withTransactionPosted(BigDecimal)} rather than mutating the shared record area that the
 * COBOL {@code REWRITE FD-ACCTFILE-REC FROM ACCOUNT-RECORD} works on
 * ({@code app/cbl/CBTRN02C.cbl:554}).
 *
 * <p>All monetary fields are {@code PIC S9(10)V99}: {@link BigDecimal} with scale 2 and ten
 * integer digits of capacity.
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

    /** Integer digits of the {@code PIC S9(10)V99} money fields on the account record. */
    private static final int MONEY_INTEGER_DIGITS = 10;
    private static final int MONEY_SCALE = 2;

    /**
     * Posts a transaction amount to the account.
     *
     * <p>Business rule BR-10, COBOL paragraph {@code 2800-UPDATE-ACCOUNT-REC}
     * ({@code app/cbl/CBTRN02C.cbl:545-552}): the amount always moves the current balance, and it
     * additionally moves the cycle-to-date credit bucket when it is positive or zero, or the
     * cycle-to-date debit bucket when it is negative.
     *
     * <p>The COBOL <em>adds</em> the signed amount to the debit bucket
     * ({@code ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT}), so a refund makes the debit bucket more
     * negative rather than increasing it by the refund's magnitude. That is reproduced here
     * deliberately; see rule BR-10a of the logic map.
     */
    public Account withTransactionPosted(BigDecimal amount) {
        BigDecimal newBalance = money(currentBalance.add(amount));
        BigDecimal newCycleCredit = amount.signum() >= 0
                ? money(currentCycleCredit.add(amount)) : currentCycleCredit;
        BigDecimal newCycleDebit = amount.signum() < 0
                ? money(currentCycleDebit.add(amount)) : currentCycleDebit;
        return new Account(id, activeStatus, newBalance, creditLimit, cashCreditLimit,
                openDate, expirationDate, reissueDate, newCycleCredit, newCycleDebit,
                addressZip, groupId);
    }

    private static BigDecimal money(BigDecimal value) {
        return CobolFixedPoint.fit(value, MONEY_INTEGER_DIGITS, MONEY_SCALE);
    }
}
