package com.carddemo.common.domain;

import java.util.List;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

public interface UserSecurityRepository extends JpaRepository<UserSecurity, String> {

    List<UserSecurity> findByUserIdGreaterThanEqualOrderByUserIdAsc(String startKey, Pageable page);

    List<UserSecurity> findByUserIdLessThanOrderByUserIdDesc(String startKey, Pageable page);
}
