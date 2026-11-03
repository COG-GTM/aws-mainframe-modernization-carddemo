package com.carddemo.auth.api.dto;

import java.time.Instant;

/**
 * Result of a signon. {@code nextScreen} reproduces the COSGN00C routing: an admin user is transferred
 * to COADM01C ({@code ADMIN}), any other user to COMEN01C ({@code MAIN}). {@code token} is the HS256
 * bearer token carrying {@code CDEMO-USER-ID} ({@code sub}) and {@code CDEMO-USER-TYPE}
 * ({@code userType}); send it as {@code Authorization: Bearer <token>} to every service.
 */
public record SignonResponse(String userId, String firstName, String lastName, String userType, String nextScreen,
                             String token, String tokenType, Instant expiresAt) {
}
