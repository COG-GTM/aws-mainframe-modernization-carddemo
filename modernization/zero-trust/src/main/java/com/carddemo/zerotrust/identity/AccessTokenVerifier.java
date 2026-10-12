package com.carddemo.zerotrust.identity;

import com.nimbusds.jose.JOSEException;
import com.nimbusds.jose.JWSAlgorithm;
import com.nimbusds.jose.crypto.ECDSAVerifier;
import com.nimbusds.jwt.JWTClaimsSet;
import com.nimbusds.jwt.SignedJWT;

import java.security.interfaces.ECPublicKey;
import java.text.ParseException;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.Date;

/**
 * Verifies access tokens on every call: algorithm pinned to ES256, signature checked
 * against the issuer's key, issuer/audience/expiry claims enforced with a small clock
 * skew allowance. Any failure is a {@link TokenRejectedException}; there is no lenient mode.
 */
public final class AccessTokenVerifier {

    private static final Duration MAX_SKEW = Duration.ofSeconds(30);

    private final ECPublicKey issuerKey;
    private final String issuer;
    private final String audience;
    private final Clock clock;

    public AccessTokenVerifier(ECPublicKey issuerKey, String issuer, String audience, Clock clock) {
        this.issuerKey = issuerKey;
        this.issuer = issuer;
        this.audience = audience;
        this.clock = clock;
    }

    public VerifiedWorkload verify(String token) {
        SignedJWT jwt;
        JWTClaimsSet claims;
        try {
            jwt = SignedJWT.parse(token);
            claims = jwt.getJWTClaimsSet();
        } catch (ParseException e) {
            throw new TokenRejectedException("unparseable access token");
        }
        // Algorithm is pinned: 'none', HS256 or any other algorithm is rejected outright,
        // before the signature is even attempted.
        if (!JWSAlgorithm.ES256.equals(jwt.getHeader().getAlgorithm())) {
            throw new TokenRejectedException("unexpected token algorithm "
                    + jwt.getHeader().getAlgorithm());
        }
        try {
            if (!jwt.verify(new ECDSAVerifier(issuerKey))) {
                throw new TokenRejectedException("bad access token signature");
            }
        } catch (JOSEException e) {
            throw new TokenRejectedException("access token signature check failed");
        }
        if (!issuer.equals(claims.getIssuer())) {
            throw new TokenRejectedException("issuer mismatch");
        }
        if (claims.getAudience() == null || !claims.getAudience().contains(audience)) {
            throw new TokenRejectedException("audience mismatch");
        }
        Instant now = clock.instant();
        Date exp = claims.getExpirationTime();
        Date nbf = claims.getNotBeforeTime();
        if (exp == null || claims.getSubject() == null || claims.getJWTID() == null) {
            throw new TokenRejectedException("token missing required claims (sub/jti/exp)");
        }
        if (now.isAfter(exp.toInstant().plus(MAX_SKEW))) {
            throw new TokenRejectedException("token expired");
        }
        if (nbf != null && now.isBefore(nbf.toInstant().minus(MAX_SKEW))) {
            throw new TokenRejectedException("token not yet valid");
        }
        return new VerifiedWorkload(claims.getSubject(), claims.getJWTID(), exp.toInstant());
    }
}
