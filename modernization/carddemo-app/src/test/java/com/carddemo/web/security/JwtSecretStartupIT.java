package com.carddemo.web.security;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.CardDemoApplication;
import org.junit.jupiter.api.Test;
import org.springframework.boot.WebApplicationType;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.context.ConfigurableApplicationContext;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

/** s6.4: the web application refuses to start with a blank or too short {@code CARDDEMO_JWT_SECRET}. */
@Testcontainers
class JwtSecretStartupIT {

    @Container
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16-alpine");

    private ConfigurableApplicationContext start(String secret) {
        return new SpringApplicationBuilder(CardDemoApplication.class).web(WebApplicationType.SERVLET)
                .profiles("test")
                .run("--server.port=0", "--spring.datasource.url=" + postgres.getJdbcUrl(),
                        "--spring.datasource.username=" + postgres.getUsername(),
                        "--spring.datasource.password=" + postgres.getPassword(),
                        "--carddemo.security.jwt.secret=" + secret);
    }

    @Test
    void aTooShortSecretStopsTheStartUp() {
        assertThatThrownBy(() -> start("x".repeat(31))).rootCause()
                .hasMessage("CARDDEMO_JWT_SECRET must be at least 32 bytes for HS256, got 31");
    }

    @Test
    void aBlankSecretStopsTheStartUp() {
        assertThatThrownBy(() -> start(" ")).rootCause().hasMessageStartingWith(
                "carddemo.security.jwt.secret is not set: export CARDDEMO_JWT_SECRET");
    }

    @Test
    void thirtyTwoBytesStart() {
        try (ConfigurableApplicationContext context = start("k".repeat(32))) {
            assertThat(context.isRunning()).isTrue();
        }
    }
}
