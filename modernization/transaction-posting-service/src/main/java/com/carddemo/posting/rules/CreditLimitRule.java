package com.carddemo.posting.rules;

import com.carddemo.mainframe.io.CobolFixedPoint;
import com.carddemo.posting.domain.Account;

import java.math.BigDecimal;

/**
 * Whether posting an amount would take an account past its credit limit.
 *
 * <p>Business rule BR-4, COBOL {@code app/cbl/CBTRN02C.cbl:403-412}.
 */
public final class CreditLimitRule {

    /** {@code WS-TEMP-BAL PIC S9(09)V99} ({@code app/cbl/CBTRN02C.cbl:187}). */
    private static final int TEMP_BALANCE_INTEGER_DIGITS = 9;
    private static final int TEMP_BALANCE_SCALE = 2;

    private CreditLimitRule() {
    }

    /**
     * The cycle-to-date exposure the transaction would create:
     * {@code ACCT-CURR-CYC-CREDIT - ACCT-CURR-CYC-DEBIT + DALYTRAN-AMT}
     * ({@code app/cbl/CBTRN02C.cbl:403-405}).
     *
     * <p>Note what this is <em>not</em>: it ignores {@code ACCT-CURR-BAL}, so the limit is checked
     * against this cycle's activity only. And because the debit bucket accumulates negative
     * amounts (see {@link Account#withTransactionPosted}), subtracting it adds the cycle's debits
     * back on rather than removing them. Both behaviours are the mainframe's; see BR-4a.
     *
     * <p>The result is computed into a {@code PIC S9(09)V99} field, two integer digits narrower
     * than the account balances that feed it, so high-order digits are dropped rather than
     * overflowing.
     */
    public static BigDecimal projectedCycleBalance(Account account, BigDecimal amount) {
        BigDecimal raw = account.currentCycleCredit()
                .subtract(account.currentCycleDebit())
                .add(amount);
        return CobolFixedPoint.fit(raw, TEMP_BALANCE_INTEGER_DIGITS, TEMP_BALANCE_SCALE);
    }

    /** {@code IF ACCT-CREDIT-LIMIT >= WS-TEMP-BAL} ({@code app/cbl/CBTRN02C.cbl:407}). */
    public static boolean permits(Account account, BigDecimal amount) {
        return account.creditLimit().compareTo(projectedCycleBalance(account, amount)) >= 0;
    }
}
