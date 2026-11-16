package com.carddemo.data.repository;

import com.carddemo.data.entity.Customer;
import org.springframework.data.jpa.repository.JpaRepository;

/** CUSTDAT: keyed READ / REWRITE by CUST-ID. */
public interface CustomerRepository extends JpaRepository<Customer, Integer> {
}
