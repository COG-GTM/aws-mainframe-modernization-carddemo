package com.carddemo.zerotrust.identity;

import com.nimbusds.jose.JOSEException;
import com.nimbusds.jose.JWSAlgorithm;
import com.nimbusds.jose.JWSHeader;
import com.nimbusds.jose.crypto.ECDSASigner;
import com.nimbusds.jose.jwk.Curve;
import com.nimbusds.jose.jwk.ECKey;
import com.nimbusds.jwt.JWTClaimsSet;
import com.nimbusds.jwt.SignedJWT;

import java.security.interfaces.ECPublicKey;
import java.time.Clock;
import java.time.Instant;
import java.util.Date;
import java.util.UUID;

/**
 * A workload's own EC P-256 key pair, generated in memory. The private key signs the
 * client assertion presented to the token issuer (private_key_jwt, RFC 7523); the
 * public half is registered with the issuer. There is no shared static secret.
 */
public final class WorkloadCredentials {

    /** Client assertions are minted with this lifetime; the issuer also caps it at 60s. */
    private static final long ASSERTION_LIFETIME_SECONDS = 30;

    private final String workloadId;
    private final ECKey key;

    private WorkloadCredentials(String workloadId, ECKey key) {
        this.workloadId = workloadId;
        this.key = key;
    }

    public static WorkloadCredentials generate(String workloadId) {
        try {
            return new WorkloadCredentials(workloadId,
                    new com.nimbusds.jose.jwk.gen.ECKeyGenerator(Curve.P_256)
                            .keyID(UUID.randomUUID().toString())
                            .generate());
        } catch (JOSEException e) {
            throw new IllegalStateException("cannot generate workload key pair", e);
        }
    }

    public String workloadId() {
        return workloadId;
    }

    public ECPublicKey publicKey() {
        try {
            return key.toECPublicKey();
        } catch (JOSEException e) {
            throw new IllegalStateException("cannot derive public key", e);
        }
    }

    /** The workload's private key, for signing assertions (kept in memory, never persisted). */
    public java.security.interfaces.ECPrivateKey keyForSigning() {
        try {
            return key.toECPrivateKey();
        } catch (JOSEException e) {
            throw new IllegalStateException("cannot derive private key", e);
        }
    }

    /** A signed JWT client assertion: iss == sub == workload id, aud == issuer id. */
    public String createClientAssertion(String issuerId, Clock clock) {
        Instant now = clock.instant();
        JWTClaimsSet claims = new JWTClaimsSet.Builder()
                .issuer(workloadId)
                .subject(workloadId)
                .audience(issuerId)
                .issueTime(Date.from(now))
                .expirationTime(Date.from(now.plusSeconds(ASSERTION_LIFETIME_SECONDS)))
                .jwtID(UUID.randomUUID().toString())
                .build();
        SignedJWT jwt = new SignedJWT(new JWSHeader.Builder(JWSAlgorithm.ES256)
                .keyID(key.getKeyID()).build(), claims);
        try {
            jwt.sign(new ECDSASigner(key.toECPrivateKey()));
        } catch (JOSEException e) {
            throw new IllegalStateException("cannot sign client assertion", e);
        }
        return jwt.serialize();
    }
}
