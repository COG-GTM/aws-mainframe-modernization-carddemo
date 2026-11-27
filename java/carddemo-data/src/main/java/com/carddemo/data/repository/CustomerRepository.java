package com.carddemo.data.repository;

import com.carddemo.data.entity.Customer;

import org.springframework.data.jpa.repository.JpaRepository;

/** Replaces CICS file CUSTDAT. */
public interface CustomerRepository extends JpaRepository<Customer, Long> {
}
