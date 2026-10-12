package com.carddemo.zerotrust.identity;

import java.time.Instant;

/** The verified contents of an access token: which workload, which token, when it expires. */
public record VerifiedWorkload(String id, String jti, Instant expiresAt) {
}
