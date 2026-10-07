package com.carddemo.web.security;

import com.carddemo.user.UserType;
import org.springframework.security.oauth2.jwt.Jwt;

/** {@code CDEMO-USER-ID} and {@code CDEMO-USER-TYPE}, taken from the verified token only (ADR-0007). */
public record CurrentUser(String userId, UserType userType) {

    public static CurrentUser of(Jwt jwt) {
        return new CurrentUser(jwt.getSubject(), UserType.valueOf(jwt.getClaimAsString(TokenService.ROLE_CLAIM)));
    }
}
