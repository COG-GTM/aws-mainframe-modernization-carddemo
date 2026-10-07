package com.carddemo.web.security;

import java.time.Instant;

/** A signed session token and its expiry. */
public record IssuedToken(String value, Instant expiresAt) {
}
