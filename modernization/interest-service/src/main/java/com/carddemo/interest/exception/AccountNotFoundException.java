package com.carddemo.interest.exception;

import com.carddemo.interest.domain.AccountId;

/**
 * No account-master record exists for an account that has category balances.
 *
 * <p>COBOL equivalent: the {@code INVALID KEY} branch of {@code 1100-GET-ACCT-DATA}
 * ({@code app/cbl/CBACT04C.cbl:372-390}), which displays {@code ACCOUNT NOT FOUND} and then
 * abends because the file status is no longer {@code '00'}.
 */
public class AccountNotFoundException extends InterestBatchException {

    private static final long serialVersionUID = 1L;

    public AccountNotFoundException(AccountId accountId) {
        super("No ACCTFILE record for account " + accountId);
    }
}
