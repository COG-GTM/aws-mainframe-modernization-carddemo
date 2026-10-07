package com.carddemo.web.signon;

import com.carddemo.web.NavigationContext;
import io.swagger.v3.oas.annotations.media.Schema;
import java.time.Instant;

/**
 * Successful sign-on (COSGN00C R-9..R-11): the token that replaces the COMMAREA user fields, and where to go next.
 *
 * @param targetMenu    {@code COADM01C} for {@code ADMIN}, {@code COMEN01C} for {@code USER}
 * @param targetMenuUrl the endpoint rendering that menu
 */
@Schema(description = "Sign-on result: bearer token + the XCTL target menu")
public record LoginResponse(
        @Schema(description = "HS256 JWT; send as 'Authorization: Bearer <token>'") String token,
        @Schema(example = "Bearer") String tokenType,
        Instant expiresAt,
        @Schema(example = "ADMIN001") String userId,
        @Schema(example = "ADMIN", allowableValues = {"ADMIN", "USER"}) String role,
        @Schema(description = "SEC-USR-TYPE level-88 code", example = "A") String userType,
        @Schema(example = "COADM01C") String targetMenu,
        @Schema(example = "/api/v1/menu/admin") String targetMenuUrl,
        NavigationContext navigation) {
}
