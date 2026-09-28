package com.carddemo.auth;

import java.time.Instant;

public record SignonResponse(
        String token,
        String tokenType,
        Instant expiresAt,
        String userId,
        String firstName,
        String lastName,
        String role,
        String nextRoute) {
}
