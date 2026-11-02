package com.carddemo.account.repository;

import com.carddemo.account.domain.AccountPosting;
import org.springframework.data.jpa.repository.JpaRepository;

public interface AccountPostingRepository extends JpaRepository<AccountPosting, String> {
}
