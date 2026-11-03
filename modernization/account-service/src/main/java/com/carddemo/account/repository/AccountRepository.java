package com.carddemo.account.repository;

import com.carddemo.account.domain.Account;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface AccountRepository extends JpaRepository<Account, Long> {

    List<Account> findByGroupId(String groupId);
}
