package com.carddemo.data.repository;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.data.entity.AppUser;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

class AppUserRepositoryIT extends RepositoryIT {

    @Autowired
    AppUserRepository users;

    @Test
    void readsUpdatesAndDeletesByUserId() {
        var admin = new AppUser();
        admin.setUserId("ADMIN001");
        admin.setFirstName("TEST");
        admin.setLastName("ADMIN");
        admin.setPasswordHash("{noop}not-a-real-hash");
        admin.setUserType("A");
        users.save(admin);
        flushAndClear();

        var read = users.findById("ADMIN001").orElseThrow();
        assertThat(read.getUserType()).isEqualTo("A");
        read.setLastName("RENAMED");
        flushAndClear();
        assertThat(users.findById("ADMIN001").orElseThrow().getLastName()).isEqualTo("RENAMED");

        users.deleteById("ADMIN001");
        flushAndClear();
        assertThat(users.findById("ADMIN001")).isEmpty();
    }
}
