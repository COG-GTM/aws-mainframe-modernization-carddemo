package com.carddemo.customer;

import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

/**
 * CUSTDATA access paths: {@code READ} by CUST-ID (COACTVWC, COACTUPC, CBSTM03A), {@code REWRITE} (COACTUPC) and the
 * sequential read in key order (CBCUS01C, CBEXPORT).
 */
public interface CustomerRepository extends JpaRepository<Customer, Integer> {

    List<Customer> findAllByOrderByCustIdAsc();
}
