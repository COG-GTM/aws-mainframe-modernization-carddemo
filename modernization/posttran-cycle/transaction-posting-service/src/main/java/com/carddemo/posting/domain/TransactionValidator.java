package com.carddemo.posting.domain;

import com.carddemo.recordio.codec.CobolNumeric;
import com.carddemo.recordio.layout.Account;
import com.carddemo.recordio.layout.CardXref;
import com.carddemo.recordio.layout.Transaction;

import java.math.BigDecimal;
import java.util.Optional;

/**
 * The validation rules of CBTRN02C 1500-VALIDATE-TRAN (app/cbl/CBTRN02C.cbl lines 371-421),
 * expressed as pure functions over the transaction and the master records it references.
 */
public final class TransactionValidator {

    /** {@code WS-TEMP-BAL PIC S9(09)V99} — one integer digit narrower than the account balances. */
    static final int TEMP_BAL_INT_DIGITS = 9;
    static final int TEMP_BAL_SCALE = 2;

    private TransactionValidator() {
    }

    /**
     * Runs the two lookups in COBOL order. Account checks run only when the card resolved. The
     * credit-limit check and the expiry check both run, and the expiry reason overwrites an
     * over-limit reason (the COBOL assigns WS-VALIDATION-FAIL-REASON twice without a guard).
     */
    public static Optional<RejectReason> validate(Transaction t, Optional<CardXref> card,
                                                  java.util.function.Function<String, Optional<Account>> accountLookup) {
        if (card.isEmpty()) {
            return Optional.of(RejectReason.INVALID_CARD);
        }
        Optional<Account> account = accountLookup.apply(card.get().accountId());
        if (account.isEmpty()) {
            return Optional.of(RejectReason.ACCOUNT_NOT_FOUND);
        }
        RejectReason reason = null;
        if (exceedsCreditLimit(account.get(), t.amount())) {
            reason = RejectReason.OVER_LIMIT;
        }
        if (receivedAfterExpiry(account.get(), t)) {
            reason = RejectReason.ACCOUNT_EXPIRED;
        }
        return Optional.ofNullable(reason);
    }

    /**
     * Lines 399-408: {@code WS-TEMP-BAL = CYC-CREDIT - CYC-DEBIT + AMT; reject if CREDIT-LIMIT < TEMP-BAL}.
     * Note that ACCT-CURR-BAL is not part of the test, and that the intermediate is stored in a
     * {@code S9(09)V99} field, so an exposure of 1,000,000,000.00 or more silently loses its
     * leading digit before the comparison. Both facts are preserved and listed in open-questions.md.
     */
    public static boolean exceedsCreditLimit(Account account, BigDecimal amount) {
        BigDecimal exposure = account.currentCycleCredit().subtract(account.currentCycleDebit()).add(amount);
        BigDecimal tempBal = CobolNumeric.truncate(exposure, TEMP_BAL_INT_DIGITS, TEMP_BAL_SCALE);
        return account.creditLimit().compareTo(tempBal) < 0;
    }

    /**
     * Lines 409-415: {@code ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS(1:10)} as a plain character
     * comparison of two yyyy-mm-dd strings. A transaction dated exactly on the expiry date posts.
     */
    public static boolean receivedAfterExpiry(Account account, Transaction t) {
        return account.expirationDate().compareTo(t.originalDate()) < 0;
    }
}
