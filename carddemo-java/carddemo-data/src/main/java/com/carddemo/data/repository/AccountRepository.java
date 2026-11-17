package com.carddemo.data.repository;

import com.carddemo.data.entity.Account;
import org.springframework.data.jpa.repository.JpaRepository;

/** ACCTDAT: keyed READ / REWRITE by ACCT-ID. */
public interface AccountRepository extends JpaRepository<Account, Long> {
}
