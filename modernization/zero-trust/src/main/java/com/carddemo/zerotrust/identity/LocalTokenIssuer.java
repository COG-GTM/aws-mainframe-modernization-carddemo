package com.carddemo.zerotrust.identity;

import com.nimbusds.jose.JOSEException;
import com.nimbusds.jose.JWSAlgorithm;
import com.nimbusds.jose.JWSHeader;
import com.nimbusds.jose.crypto.ECDSASigner;
import com.nimbusds.jose.crypto.ECDSAVerifier;
import com.nimbusds.jose.jwk.Curve;
import com.nimbusds.jose.jwk.ECKey;
import com.nimbusds.jose.jwk.JWKSet;
import com.nimbusds.jose.jwk.gen.ECKeyGenerator;
import com.nimbusds.jwt.JWTClaimsSet;
import com.nimbusds.jwt.SignedJWT;

import java.security.interfaces.ECPublicKey;
import java.text.ParseException;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.Date;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

/**
 * A local, in-memory token issuer: the workload-identity half of the secured posting
 * path. Workloads authenticate with a signed client assertion (private_key_jwt, RFC 7523)
 * and receive a short-lived ES256 access token. There are no network calls and no shared
 * static secrets; each workload holds its own key pair and the issuer holds its own.
 *
 * <p>The issuer key pair is generated at construction and never persisted — this class is
 * for tests and local runs. In production the issuer is the platform OIDC/ICAM provider
 * and workload keys come from the platform workload-identity system.
 */
public final class LocalTokenIssuer {

    public static final String DEFAULT_ISSUER_ID = "urn:carddemo:local-issuer";
    /** Access-token lifetime ceiling; anything longer is refused at configuration time. */
    public static final Duration MAX_TOKEN_TTL = Duration.ofMinutes(15);
    /** Maximum accepted client-assertion lifetime (exp minus iat). */
    public static final Duration MAX_ASSERTION_LIFETIME = Duration.ofSeconds(60);

    private final String issuerId;
    private final Duration tokenTtl;
    private final Clock clock;
    private final ECKey signingKey;
    private final Map<String, ECPublicKey> workloads = new ConcurrentHashMap<>();
    private final Set<String> seenAssertionIds = ConcurrentHashMap.newKeySet();

    public LocalTokenIssuer(String issuerId, Duration tokenTtl, Clock clock) {
        if (tokenTtl.compareTo(MAX_TOKEN_TTL) > 0) {
            throw new IllegalArgumentException(
                    "token ttl " + tokenTtl + " exceeds the " + MAX_TOKEN_TTL + " maximum");
        }
        this.issuerId = issuerId;
        this.tokenTtl = tokenTtl;
        this.clock = clock;
        try {
            this.signingKey = new ECKeyGenerator(Curve.P_256).keyID(UUID.randomUUID().toString()).generate();
        } catch (JOSEException e) {
            throw new IllegalStateException("cannot generate issuer key pair", e);
        }
    }

    public String issuerId() {
        return issuerId;
    }

    public void registerWorkload(String workloadId, ECPublicKey publicKey) {
        workloads.put(workloadId, publicKey);
    }

    /** The issuer's verification key, exposed so verifiers pin to it explicitly. */
    public ECPublicKey verificationKey() {
        try {
            return signingKey.toECPublicKey();
        } catch (JOSEException e) {
            throw new IllegalStateException("cannot derive public key", e);
        }
    }

    /** Public half of the issuer key, as a JWKS document. */
    public JWKSet publicJwkSet() {
        return new JWKSet(signingKey.toPublicJWK());
    }

    /**
     * OIDC client-credentials style exchange: verify the private_key_jwt client
     * assertion, then mint an access token for {@code audience}.
     */
    public String issue(String clientAssertionJwt, String audience) {
        SignedJWT assertion;
        JWTClaimsSet claims;
        try {
            assertion = SignedJWT.parse(clientAssertionJwt);
            claims = assertion.getJWTClaimsSet();
        } catch (ParseException e) {
            throw new TokenRejectedException("unparseable client assertion");
        }
        if (!JWSAlgorithm.ES256.equals(assertion.getHeader().getAlgorithm())) {
            throw new TokenRejectedException("client assertion must be ES256");
        }
        String sub = claims.getSubject();
        ECPublicKey workloadKey = sub == null ? null : workloads.get(sub);
        if (workloadKey == null) {
            throw new TokenRejectedException("unregistered workload '" + sub + "'");
        }
        try {
            if (!assertion.verify(new ECDSAVerifier(workloadKey))) {
                throw new TokenRejectedException("bad client assertion signature");
            }
        } catch (JOSEException e) {
            throw new TokenRejectedException("client assertion signature check failed");
        }
        if (!sub.equals(claims.getIssuer())) {
            throw new TokenRejectedException("client assertion iss != sub");
        }
        if (claims.getAudience() == null || !claims.getAudience().contains(issuerId)) {
            throw new TokenRejectedException("client assertion audience mismatch");
        }
        Instant now = clock.instant();
        Date exp = claims.getExpirationTime();
        Date iat = claims.getIssueTime();
        if (exp == null || iat == null) {
            throw new TokenRejectedException("client assertion missing iat/exp");
        }
        if (Duration.between(iat.toInstant(), exp.toInstant()).compareTo(MAX_ASSERTION_LIFETIME) > 0) {
            throw new TokenRejectedException("client assertion lifetime exceeds 60s");
        }
        if (!exp.toInstant().isAfter(now)) {
            throw new TokenRejectedException("client assertion expired");
        }
        String jti = claims.getJWTID();
        if (jti == null || !seenAssertionIds.add(jti)) {
            throw new TokenRejectedException("client assertion jti missing or replayed");
        }

        Instant issuedAt = clock.instant();
        JWTClaimsSet token = new JWTClaimsSet.Builder()
                .issuer(issuerId)
                .subject(sub)
                .audience(audience)
                .issueTime(Date.from(issuedAt))
                .notBeforeTime(Date.from(issuedAt))
                .expirationTime(Date.from(issuedAt.plus(tokenTtl)))
                .jwtID(UUID.randomUUID().toString())
                .claim("kind", "workload")
                .build();
        SignedJWT accessToken = new SignedJWT(new JWSHeader.Builder(JWSAlgorithm.ES256)
                .keyID(signingKey.getKeyID()).build(), token);
        try {
            accessToken.sign(new ECDSASigner(signingKey.toECPrivateKey()));
        } catch (JOSEException e) {
            throw new IllegalStateException("cannot sign access token", e);
        }
        return accessToken.serialize();
    }
}
