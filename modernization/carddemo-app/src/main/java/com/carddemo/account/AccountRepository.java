package com.carddemo.account;

import java.util.List;
import org.springframework.data.domain.Limit;
import org.springframework.data.jpa.repository.JpaRepository;

/**
 * ACCTDATA access paths: {@code READ}/{@code READ UPDATE} by ACCT-ID (COACTVWC, COACTUPC, COBIL00C, CBTRN01C/02C,
 * CBACT04C), {@code REWRITE}, and the sequential read in key order (CBACT01C, CBEXPORT).
 */
public interface AccountRepository extends JpaRepository<Account, Long> {

    List<Account> findAllByOrderByAcctIdAsc();

    /** Keyset browse in key order (batch sequential read of ACCTDATA). */
    List<Account> findByAcctIdGreaterThanOrderByAcctIdAsc(long lastAcctId, Limit limit);
}
