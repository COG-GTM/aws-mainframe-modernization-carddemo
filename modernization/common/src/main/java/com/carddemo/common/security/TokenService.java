package com.carddemo.common.security;

import com.nimbusds.jose.JOSEException;
import com.nimbusds.jose.JWSAlgorithm;
import com.nimbusds.jose.JWSHeader;
import com.nimbusds.jose.crypto.MACSigner;
import com.nimbusds.jose.crypto.MACVerifier;
import com.nimbusds.jwt.JWTClaimsSet;
import com.nimbusds.jwt.SignedJWT;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.Date;
import java.util.Set;
import java.util.UUID;

/**
 * Issues and verifies the HS256 JWT that replaces the COMMAREA user fields:
 * {@code sub} = {@code CDEMO-USER-ID}, {@code userType} = {@code CDEMO-USER-TYPE} ({@code A}/{@code U}).
 */
public class TokenService {

    public static final String USER_TYPE_CLAIM = "userType";
    private static final Set<String> USER_TYPES = Set.of(CallerIdentity.ADMIN, CallerIdentity.USER);

    private final byte[] secret;
    private final Duration ttl;
    private final String issuer;
    private final Clock clock;

    public TokenService(SecurityProperties properties, Clock clock) {
        if (properties.tokenSecret() == null || properties.tokenSecret().isBlank()) {
            throw new IllegalStateException("carddemo.security.token-secret (CARDDEMO_TOKEN_SECRET) is not set");
        }
        this.secret = properties.tokenSecret().getBytes(StandardCharsets.UTF_8);
        if (secret.length < 32) {
            throw new IllegalStateException("carddemo.security.token-secret must be at least 32 bytes for HS256");
        }
        this.ttl = properties.tokenTtl();
        this.issuer = properties.issuer();
        this.clock = clock;
    }

    public record IssuedToken(String token, Instant expiresAt) {
    }

    public IssuedToken issue(String userId, String userType) {
        Instant now = clock.instant();
        Instant expiresAt = now.plus(ttl);
        JWTClaimsSet claims = new JWTClaimsSet.Builder()
                .jwtID(UUID.randomUUID().toString())
                .issuer(issuer)
                .subject(userId)
                .claim(USER_TYPE_CLAIM, userType)
                .issueTime(Date.from(now))
                .expirationTime(Date.from(expiresAt))
                .build();
        SignedJWT jwt = new SignedJWT(new JWSHeader(JWSAlgorithm.HS256), claims);
        try {
            jwt.sign(new MACSigner(secret));
        } catch (JOSEException e) {
            throw new IllegalStateException("Cannot sign token", e);
        }
        return new IssuedToken(jwt.serialize(), expiresAt);
    }

    public CallerIdentity verify(String token) {
        try {
            SignedJWT jwt = SignedJWT.parse(token);
            if (!JWSAlgorithm.HS256.equals(jwt.getHeader().getAlgorithm())
                    || !jwt.verify(new MACVerifier(secret))) {
                throw new InvalidTokenException("bad signature", null);
            }
            JWTClaimsSet claims = jwt.getJWTClaimsSet();
            Date expires = claims.getExpirationTime();
            if (expires == null || !expires.toInstant().isAfter(clock.instant())) {
                throw new InvalidTokenException("expired", null);
            }
            if (!issuer.equals(claims.getIssuer())) {
                throw new InvalidTokenException("wrong issuer", null);
            }
            String userId = claims.getSubject();
            String userType = claims.getStringClaim(USER_TYPE_CLAIM);
            if (userId == null || userId.isBlank() || !USER_TYPES.contains(userType)) {
                throw new InvalidTokenException("missing claims", null);
            }
            return new CallerIdentity(userId, userType);
        } catch (ParseException | JOSEException e) {
            throw new InvalidTokenException("unreadable token", e);
        }
    }
}
