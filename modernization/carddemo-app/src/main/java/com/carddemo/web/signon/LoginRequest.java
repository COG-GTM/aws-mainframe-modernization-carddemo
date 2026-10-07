package com.carddemo.web.signon;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Size;

/**
 * The two input fields of map {@code COSGN0A}: {@code USERIDI PIC X(8)}, {@code PASSWDI PIC X(8)}. Blank or missing
 * values are not a validation error here: COSGN00C answers them with its own messages (R-5, R-6).
 */
@Schema(description = "Sign-on input (COSGN0A USERID/PASSWD); case-insensitive (COSGN00C R-7)")
public record LoginRequest(
        @Schema(example = "ADMIN001", maxLength = 8) @Size(max = 8, message = "User ID can be at most 8 characters")
        String userId,
        @Schema(example = "PASSWORD", maxLength = 8, format = "password")
        @Size(max = 8, message = "Password can be at most 8 characters")
        String password) {

    @Override
    public String toString() {
        return "LoginRequest[userId=" + userId + ", password=***]";
    }
}
