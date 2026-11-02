package com.carddemo.common.security;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;
import org.junit.jupiter.api.Test;

class TokenServiceTest {

    private static final String SECRET = "unit-test-token-secret-0123456789abcdef";
    private static final Instant NOW = Instant.parse("2024-05-01T10:00:00Z");

    private static TokenService service(String secret, Instant now) {
        return new TokenService(new SecurityProperties(secret, Duration.ofHours(8), "carddemo-auth-service",
                List.of()), Clock.fixed(now, ZoneOffset.UTC));
    }

    @Test
    void roundTripsUserIdAndUserType() {
        TokenService tokens = service(SECRET, NOW);
        TokenService.IssuedToken issued = tokens.issue("ADMIN001", "A");

        assertThat(issued.expiresAt()).isEqualTo(NOW.plus(Duration.ofHours(8)));
        CallerIdentity caller = tokens.verify(issued.token());
        assertThat(caller).isEqualTo(new CallerIdentity("ADMIN001", "A"));
        assertThat(caller.isAdmin()).isTrue();
        assertThat(tokens.verify(tokens.issue("USER0001", "U").token()).isAdmin()).isFalse();
    }

    @Test
    void rejectsATokenSignedWithAnotherSecret() {
        String token = service("another-secret-of-at-least-32-bytes-xx", NOW).issue("ADMIN001", "A").token();
        assertThatThrownBy(() -> service(SECRET, NOW).verify(token)).isInstanceOf(InvalidTokenException.class);
    }

    @Test
    void rejectsAnExpiredToken() {
        String token = service(SECRET, NOW).issue("ADMIN001", "A").token();
        assertThatThrownBy(() -> service(SECRET, NOW.plus(Duration.ofHours(9))).verify(token))
                .isInstanceOf(InvalidTokenException.class);
    }

    @Test
    void rejectsAnUnknownUserTypeAndGarbage() {
        TokenService tokens = service(SECRET, NOW);
        assertThatThrownBy(() -> tokens.verify(tokens.issue("ADMIN001", "X").token()))
                .isInstanceOf(InvalidTokenException.class);
        assertThatThrownBy(() -> tokens.verify("not-a-jwt")).isInstanceOf(InvalidTokenException.class);
    }

    @Test
    void refusesAMissingOrShortSecret() {
        assertThatThrownBy(() -> service("", NOW)).isInstanceOf(IllegalStateException.class);
        assertThatThrownBy(() -> service("too-short", NOW)).isInstanceOf(IllegalStateException.class);
    }
}
