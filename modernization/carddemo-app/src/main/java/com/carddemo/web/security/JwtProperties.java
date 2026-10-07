package com.carddemo.web.security;

import java.time.Duration;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Signing settings of the session token (ADR-0017).
 *
 * @param secret HS256 key, at least 32 bytes; from {@code CARDDEMO_JWT_SECRET}, no default in any
 *               profile (s6.4)
 * @param issuer {@code iss} claim written and required
 * @param ttl    lifetime of a token
 */
@ConfigurationProperties("carddemo.security.jwt")
public record JwtProperties(String secret, String issuer, Duration ttl) {

    public JwtProperties {
        issuer = issuer == null || issuer.isBlank() ? "carddemo" : issuer;
        ttl = ttl == null ? Duration.ofHours(1) : ttl;
    }
}
