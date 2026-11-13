package com.carddemo.data.repository;

import com.carddemo.data.entity.Account;

import org.springframework.data.jpa.repository.JpaRepository;

/** Replaces CICS READ/REWRITE on ACCTDAT. */
public interface AccountRepository extends JpaRepository<Account, Long> {
}
