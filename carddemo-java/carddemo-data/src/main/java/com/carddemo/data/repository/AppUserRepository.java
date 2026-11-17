package com.carddemo.data.repository;

import com.carddemo.data.entity.AppUser;
import org.springframework.data.jpa.repository.JpaRepository;

/** USRSEC: keyed READ by SEC-USR-ID (sign-on, user list / add / update / delete). */
public interface AppUserRepository extends JpaRepository<AppUser, String> {
}
