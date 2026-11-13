package com.carddemo.data.repository;

import com.carddemo.data.entity.UserSecurity;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

/** Replaces CICS file USRSEC. */
public interface UserSecurityRepository extends JpaRepository<UserSecurity, String> {

    /** STARTBR/READNEXT browse used by the user list screen (COUSR00C). */
    Page<UserSecurity> findByUserIdGreaterThanEqualOrderByUserId(String userId, Pageable pageable);
}
