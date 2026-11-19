package com.carddemo.account.repository;

import com.carddemo.account.domain.Account;
import org.springframework.data.jpa.repository.JpaRepository;

/** Table {@code account.accounts} (ACCTDATA). */
public interface AccountRepository extends JpaRepository<Account, Long> {
}
