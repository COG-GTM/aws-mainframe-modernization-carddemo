package com.carddemo.posting.rules;

import com.carddemo.posting.domain.Account;
import com.carddemo.posting.domain.DailyTransaction;

/**
 * Whether a transaction reached the posting run after its account expired.
 *
 * <p>Business rule BR-5, COBOL {@code IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)}
 * ({@code app/cbl/CBTRN02C.cbl:414}).
 */
public final class AccountExpiryRule {

    private AccountExpiryRule() {
    }

    /**
     * Compares the account expiry date with the transaction's origination date.
     *
     * <p>The COBOL comparison is alphanumeric, not a date comparison: two {@code PIC X(10)} fields
     * are compared byte by byte in the EBCDIC collating sequence. Both values are
     * {@code YYYY-MM-DD}, where every byte is either a digit or a hyphen and hyphen sorts below
     * the digits in both EBCDIC and Unicode, so a plain {@link String#compareTo} gives the same
     * answer. It also inherits the same weakness: a malformed or blank date is not detected, it
     * simply sorts low. See BR-5a.
     */
    public static boolean expiredFor(Account account, DailyTransaction transaction) {
        return account.expirationDate().compareTo(transaction.originationDate()) < 0;
    }
}
