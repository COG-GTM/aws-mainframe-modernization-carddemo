package com.carddemo.customer;

import java.util.List;
import org.springframework.data.domain.Limit;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

/**
 * CUSTDATA access paths: {@code READ} by CUST-ID (COACTVWC, COACTUPC, CBSTM03A), {@code REWRITE} (COACTUPC) and the
 * sequential read in key order (CBCUS01C, CBEXPORT).
 */
public interface CustomerRepository extends JpaRepository<Customer, Integer> {

    List<Customer> findAllByOrderByCustIdAsc();

    /** Keyset browse in key order (batch sequential read of CUSTDATA). */
    List<Customer> findByCustIdGreaterThanOrderByCustIdAsc(int lastCustId, Limit limit);

    /**
     * {@code READ FILE('CUSTDAT') UPDATE}: locks the row until the transaction ends and returns its committed version,
     * bypassing the persistence context so a concurrent commit since the entity was loaded is seen.
     */
    @Query(value = "select version from customer where cust_id = :custId for update", nativeQuery = true)
    Optional<Long> lockVersion(@Param("custId") int custId);
}
