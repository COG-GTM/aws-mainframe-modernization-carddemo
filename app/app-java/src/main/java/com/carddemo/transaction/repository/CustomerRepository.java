package com.carddemo.transaction.repository;

import com.carddemo.transaction.domain.Customer;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CustomerRepository extends JpaRepository<Customer, Long> {
}
