package com.carddemo.online;

import com.carddemo.data.repository.AccountRepository;
import com.carddemo.data.testing.PostgresTestcontainersConfiguration;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.context.annotation.Import;
import org.testcontainers.junit.jupiter.Testcontainers;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Import(PostgresTestcontainersConfiguration.class)
@Testcontainers(disabledWithoutDocker = true)
class CardDemoOnlineApplicationTest {

    @Autowired
    TestRestTemplate http;

    @Autowired
    AccountRepository accounts;

    @Test
    void startsWithDataLayerAndHealthEndpoint() {
        assertThat(accounts.count()).isZero();
        assertThat(http.getForObject("/actuator/health", String.class)).contains("\"status\":\"UP\"");
    }
}
