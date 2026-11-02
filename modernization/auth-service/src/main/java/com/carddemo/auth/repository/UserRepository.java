package com.carddemo.auth.repository;

import com.carddemo.auth.domain.User;
import java.util.List;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

public interface UserRepository extends JpaRepository<User, String> {

    /** COUSR00C STARTBR on SEC-USR-ID + READNEXT: users from the start key forward, in key order. */
    List<User> findByUserIdGreaterThanEqualOrderByUserIdAsc(String startKey, Pageable pageable);

    /** COUSR00C READPREV: users before the key, nearest first. */
    List<User> findByUserIdLessThanOrderByUserIdDesc(String key, Pageable pageable);
}
