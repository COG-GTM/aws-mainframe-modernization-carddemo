package com.carddemo.interest.exception;

import com.carddemo.interest.domain.AccountId;

/**
 * No card cross-reference exists for an account being processed.
 *
 * <p>COBOL equivalent: the {@code INVALID KEY} branch of {@code 1110-GET-XREF-DATA}
 * ({@code app/cbl/CBACT04C.cbl:393-412}), which abends the job.
 */
public class CardXrefNotFoundException extends InterestBatchException {

    private static final long serialVersionUID = 1L;

    public CardXrefNotFoundException(AccountId accountId) {
        super("No XREFFILE record for account " + accountId);
    }
}
