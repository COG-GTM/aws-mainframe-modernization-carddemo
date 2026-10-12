package com.carddemo.zerotrust.identity;

import com.nimbusds.jwt.SignedJWT;

import java.text.ParseException;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;

/**
 * Supplies the current access token for this workload, transparently re-running the
 * client-assertion exchange when the cached token has less than 60 seconds of life left.
 */
public final class WorkloadTokenSource {

    private static final Duration REFRESH_WINDOW = Duration.ofSeconds(60);

    private final WorkloadCredentials credentials;
    private final LocalTokenIssuer issuer;
    private final String audience;
    private final Clock clock;

    private String cachedToken;
    private Instant cachedExpiry = Instant.EPOCH;

    public WorkloadTokenSource(WorkloadCredentials credentials, LocalTokenIssuer issuer,
                               String audience, Clock clock) {
        this.credentials = credentials;
        this.issuer = issuer;
        this.audience = audience;
        this.clock = clock;
    }

    public synchronized String token() {
        if (cachedToken != null
                && clock.instant().plus(REFRESH_WINDOW).isBefore(cachedExpiry)) {
            return cachedToken;
        }
        String assertion = credentials.createClientAssertion(issuer.issuerId(), clock);
        cachedToken = issuer.issue(assertion, audience);
        try {
            cachedExpiry = SignedJWT.parse(cachedToken).getJWTClaimsSet().getExpirationTime().toInstant();
        } catch (ParseException e) {
            throw new IllegalStateException("issuer returned an unparseable token", e);
        }
        return cachedToken;
    }
}
