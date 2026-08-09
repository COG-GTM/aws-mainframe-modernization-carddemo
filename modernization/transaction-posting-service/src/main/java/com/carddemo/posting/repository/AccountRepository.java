package com.carddemo.posting.repository;

import com.carddemo.posting.domain.Account;
import com.carddemo.posting.domain.AccountId;

import java.util.Optional;

/**
 * The account master ({@code ACCTFILE} DD, {@code AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS},
 * {@code app/jcl/POSTTRAN.jcl:40-41}), opened {@code I-O} by the posting program.
 *
 * <p>Replaces the keyed {@code READ ACCOUNT-FILE} of {@code 1500-B-LOOKUP-ACCT}
 * ({@code app/cbl/CBTRN02C.cbl:393-421}) and the {@code REWRITE} of
 * {@code 2800-UPDATE-ACCOUNT-REC} ({@code app/cbl/CBTRN02C.cbl:554}).
 */
public interface AccountRepository {

    Optional<Account> findById(AccountId accountId);

    /** Rewrites an account in place; the account must already exist. */
    void save(Account account);
}
