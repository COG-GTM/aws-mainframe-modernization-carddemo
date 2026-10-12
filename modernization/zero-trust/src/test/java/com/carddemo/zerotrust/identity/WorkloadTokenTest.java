package com.carddemo.zerotrust.identity;

import com.nimbusds.jose.JWSAlgorithm;
import com.nimbusds.jose.JWSHeader;
import com.nimbusds.jose.crypto.ECDSASigner;
import com.nimbusds.jose.crypto.MACSigner;
import com.nimbusds.jose.jwk.gen.ECKeyGenerator;
import com.nimbusds.jose.jwk.Curve;
import com.nimbusds.jwt.JWTClaimsSet;
import com.nimbusds.jwt.PlainJWT;
import com.nimbusds.jwt.SignedJWT;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.time.Instant;
import java.util.Date;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class WorkloadTokenTest {

    private static final String ISSUER_ID = "urn:carddemo:local-issuer";
    private static final String AUDIENCE = "urn:carddemo:posting";
    private static final Instant T0 = Instant.parse("2025-01-01T00:00:00Z");

    private final MutableClock clock = new MutableClock(T0);
    private final LocalTokenIssuer issuer = new LocalTokenIssuer(ISSUER_ID, Duration.ofMinutes(5), clock);
    private final WorkloadCredentials creds = WorkloadCredentials.generate("svc-posttran-post");
    private final AccessTokenVerifier verifier =
            new AccessTokenVerifier(issuer.verificationKey(), ISSUER_ID, AUDIENCE, clock);

    {
        issuer.registerWorkload(creds.workloadId(), creds.publicKey());
    }

    private String freshToken() {
        return issuer.issue(creds.createClientAssertion(ISSUER_ID, clock), AUDIENCE);
    }

    @Test
    void validTokenVerifies() {
        VerifiedWorkload w = verifier.verify(freshToken());
        assertThat(w.id()).isEqualTo("svc-posttran-post");
        assertThat(w.jti()).isNotBlank();
        assertThat(w.expiresAt()).isEqualTo(T0.plusSeconds(300));
    }

    @Test
    void expiredTokenIsRejected() {
        String token = freshToken();
        clock.advance(Duration.ofMinutes(5).plusSeconds(31));
        assertThatThrownBy(() -> verifier.verify(token))
                .isInstanceOf(TokenRejectedException.class)
                .hasMessageContaining("expired");
    }

    @Test
    void forgedTokenIsRejected() throws Exception {
        // A token signed by an attacker key is not the issuer's signature.
        var attacker = new ECKeyGenerator(Curve.P_256).generate();
        SignedJWT forged = new SignedJWT(
                new JWSHeader.Builder(JWSAlgorithm.ES256).build(),
                claims("svc-posttran-post", T0, T0.plusSeconds(300)).build());
        forged.sign(new ECDSASigner(attacker));
        assertThatThrownBy(() -> verifier.verify(forged.serialize()))
                .isInstanceOf(TokenRejectedException.class);
    }

    @Test
    void tokenFromAnotherIssuerInstanceIsRejected() {
        LocalTokenIssuer other = new LocalTokenIssuer(ISSUER_ID, Duration.ofMinutes(5), clock);
        WorkloadCredentials otherCreds = WorkloadCredentials.generate("svc-posttran-post");
        other.registerWorkload(otherCreds.workloadId(), otherCreds.publicKey());
        String token = other.issue(otherCreds.createClientAssertion(ISSUER_ID, clock), AUDIENCE);
        assertThatThrownBy(() -> verifier.verify(token))
                .isInstanceOf(TokenRejectedException.class);
    }

    @Test
    void tamperedPayloadIsRejected() throws Exception {
        String token = freshToken();
        String[] parts = token.split("\\.");
        // Re-sign a modified payload with a different key — signature cannot match.
        SignedJWT parsed = SignedJWT.parse(token);
        JWTClaimsSet changed = new JWTClaimsSet.Builder(parsed.getJWTClaimsSet())
                .subject("svc-posttran-report").build();
        var attacker = new ECKeyGenerator(Curve.P_256).generate();
        SignedJWT tampered = new SignedJWT(
                new JWSHeader.Builder(JWSAlgorithm.ES256).build(), changed);
        tampered.sign(new ECDSASigner(attacker));
        assertThatThrownBy(() -> verifier.verify(tampered.serialize()))
                .isInstanceOf(TokenRejectedException.class);
        assertThat(parts).hasSize(3);
    }

    @Test
    void wrongAudienceIsRejected() {
        String token = issuer.issue(creds.createClientAssertion(ISSUER_ID, clock),
                "urn:carddemo:something-else");
        assertThatThrownBy(() -> verifier.verify(token))
                .isInstanceOf(TokenRejectedException.class)
                .hasMessageContaining("audience");
    }

    @Test
    void unsignedTokenIsRejected() {
        PlainJWT plain = new PlainJWT(claims("svc-posttran-post", T0, T0.plusSeconds(300)).build());
        assertThatThrownBy(() -> verifier.verify(plain.serialize()))
                .isInstanceOf(TokenRejectedException.class);
    }

    @Test
    void hs256TokenIsRejected() throws Exception {
        byte[] secret = new byte[32];
        SignedJWT hs = new SignedJWT(new JWSHeader(JWSAlgorithm.HS256),
                claims("svc-posttran-post", T0, T0.plusSeconds(300)).build());
        hs.sign(new MACSigner(secret));
        assertThatThrownBy(() -> verifier.verify(hs.serialize()))
                .isInstanceOf(TokenRejectedException.class);
    }

    @Test
    void clientAssertionForUnregisteredWorkloadIsRejected() {
        WorkloadCredentials rogue = WorkloadCredentials.generate("svc-rogue");
        assertThatThrownBy(() -> issuer.issue(
                rogue.createClientAssertion(ISSUER_ID, clock), AUDIENCE))
                .isInstanceOf(TokenRejectedException.class)
                .hasMessageContaining("unregistered");
    }

    @Test
    void clientAssertionSignedWithWrongKeyIsRejected() throws Exception {
        // Register svc-x with key A, then present an assertion for svc-x signed by key B.
        WorkloadCredentials a = WorkloadCredentials.generate("svc-x");
        WorkloadCredentials b = WorkloadCredentials.generate("svc-y");
        issuer.registerWorkload(a.workloadId(), a.publicKey());
        // forge: claims of svc-x, signed by b's key
        SignedJWT forged = new SignedJWT(new JWSHeader.Builder(JWSAlgorithm.ES256).build(),
                new JWTClaimsSet.Builder()
                        .issuer("svc-x").subject("svc-x").audience(ISSUER_ID)
                        .issueTime(Date.from(T0))
                        .expirationTime(Date.from(T0.plusSeconds(30)))
                        .jwtID(UUID.randomUUID().toString()).build());
        forged.sign(new ECDSASigner(b.keyForSigning()));
        assertThatThrownBy(() -> issuer.issue(forged.serialize(), AUDIENCE))
                .isInstanceOf(TokenRejectedException.class);
    }

    @Test
    void replayedClientAssertionIsRejected() {
        String assertion = creds.createClientAssertion(ISSUER_ID, clock);
        issuer.issue(assertion, AUDIENCE);
        assertThatThrownBy(() -> issuer.issue(assertion, AUDIENCE))
                .isInstanceOf(TokenRejectedException.class)
                .hasMessageContaining("replayed");
    }

    @Test
    void ttlAboveMaximumIsRefused() {
        assertThatThrownBy(() -> new LocalTokenIssuer(ISSUER_ID, Duration.ofMinutes(16), clock))
                .isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void tokenSourceRefreshesNearExpiry() {
        WorkloadTokenSource source = new WorkloadTokenSource(creds, issuer, AUDIENCE, clock);
        String first = source.token();
        assertThat(source.token()).isEqualTo(first);
        // move within the 60s refresh window
        clock.advance(Duration.ofMinutes(5).minusSeconds(30));
        String refreshed = source.token();
        assertThat(refreshed).isNotEqualTo(first);
        assertThat(verifier.verify(refreshed).id()).isEqualTo("svc-posttran-post");
    }

    private static JWTClaimsSet.Builder claims(String sub, Instant iat, Instant exp) {
        return new JWTClaimsSet.Builder()
                .issuer(ISSUER_ID).subject(sub).audience(AUDIENCE)
                .issueTime(Date.from(iat))
                .notBeforeTime(Date.from(iat))
                .expirationTime(Date.from(exp))
                .jwtID(UUID.randomUUID().toString())
                .claim("kind", "workload");
    }
}
