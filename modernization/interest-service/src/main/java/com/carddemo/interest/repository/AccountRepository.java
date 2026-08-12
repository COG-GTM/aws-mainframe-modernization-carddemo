package com.carddemo.interest.repository;

import com.carddemo.interest.domain.Account;
import com.carddemo.interest.domain.AccountId;

import java.util.Optional;

/**
 * Access to the account master ({@code ACCTFILE} DD, {@code AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS}).
 *
 * <p>Replaces the keyed {@code READ ACCOUNT-FILE} of {@code 1100-GET-ACCT-DATA}
 * ({@code app/cbl/CBACT04C.cbl:372-376}).
 */
public interface AccountRepository {

    Optional<Account> findById(AccountId accountId);
}
