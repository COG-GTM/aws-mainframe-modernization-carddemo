package com.carddemo.auth.repository;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.auth.domain.User;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.PageRequest;
import org.springframework.transaction.annotation.Transactional;

@SpringBootTest
@Transactional
class UserRepositoryTest {

    @Autowired
    private UserRepository users;

    @BeforeEach
    void setUp() {
        users.deleteAll();
        for (String id : List.of("USER0001", "USER0002", "USER0003", "USER0004", "ADMIN001")) {
            users.save(new User(id, "hash", id.startsWith("ADMIN") ? "A" : "U"));
        }
    }

    @Test
    void browsesForwardFromStartKeyInKeyOrder() {
        assertThat(users.findByUserIdGreaterThanEqualOrderByUserIdAsc("USER0002", PageRequest.ofSize(2)))
                .extracting(User::getUserId).containsExactly("USER0002", "USER0003");
    }

    @Test
    void browsesBackwardFromKeyNearestFirst() {
        assertThat(users.findByUserIdLessThanOrderByUserIdDesc("USER0003", PageRequest.ofSize(10)))
                .extracting(User::getUserId).containsExactly("USER0002", "USER0001", "ADMIN001");
    }
}
