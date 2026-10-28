package com.carddemo.recordio.layout;

import java.math.BigDecimal;

/**
 * {@code app/cpy/CVACT01Y.cpy ACCOUNT-RECORD}, RECLN 300.
 *
 * <p>Dates are the 10-character {@code X(10)} text the programs compare lexically
 * (e.g. {@code ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS(1:10)}), so they stay as strings here.
 * Money is {@code S9(10)V99} everywhere: 10 integer digits, 2 decimals.
 */
public record Account(
        String accountId,
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

    public static final int LENGTH = 300;
    public static final int MONEY_INT_DIGITS = 10;
    public static final int MONEY_SCALE = 2;

    public Account withBalances(BigDecimal balance, BigDecimal cycleCredit, BigDecimal cycleDebit) {
        return new Account(accountId, activeStatus, balance, creditLimit, cashCreditLimit, openDate,
                expirationDate, reissueDate, cycleCredit, cycleDebit, addressZip, groupId);
    }
}
