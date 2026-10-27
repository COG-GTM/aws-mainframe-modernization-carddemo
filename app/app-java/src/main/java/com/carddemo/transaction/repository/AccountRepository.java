package com.carddemo.transaction.repository;

import com.carddemo.transaction.domain.Account;
import org.springframework.data.jpa.repository.JpaRepository;

public interface AccountRepository extends JpaRepository<Account, Long> {
}
