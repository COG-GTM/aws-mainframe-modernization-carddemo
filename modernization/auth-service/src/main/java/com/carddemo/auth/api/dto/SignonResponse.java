package com.carddemo.auth.api.dto;

/**
 * Result of a signon. {@code menu} reproduces the COSGN00C routing: an admin user is transferred
 * to COADM01C, any other user to COMEN01C. Session tokens are not issued yet; the SPA carries the
 * user id until the token issuer is added (see docs/API-CONTRACT.md).
 */
public record SignonResponse(String userId, String firstName, String lastName, String userType, String menu) {
}
