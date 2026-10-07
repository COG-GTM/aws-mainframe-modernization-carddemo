package com.carddemo.account;

import java.util.List;
import org.springframework.data.domain.Limit;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

/**
 * ACCTDATA access paths: {@code READ}/{@code READ UPDATE} by ACCT-ID (COACTVWC, COACTUPC, COBIL00C, CBTRN01C/02C,
 * CBACT04C), {@code REWRITE}, and the sequential read in key order (CBACT01C, CBEXPORT).
 */
public interface AccountRepository extends JpaRepository<Account, Long> {

    List<Account> findAllByOrderByAcctIdAsc();

    /** Keyset browse in key order (batch sequential read of ACCTDATA). */
    List<Account> findByAcctIdGreaterThanOrderByAcctIdAsc(long lastAcctId, Limit limit);

    /**
     * {@code READ FILE('ACCTDAT') UPDATE}: locks the row until the transaction ends and returns its committed version,
     * bypassing the persistence context so a concurrent commit since the entity was loaded is seen.
     */
    @Query(value = "select version from account where acct_id = :acctId for update", nativeQuery = true)
    Optional<Long> lockVersion(@Param("acctId") long acctId);
}
