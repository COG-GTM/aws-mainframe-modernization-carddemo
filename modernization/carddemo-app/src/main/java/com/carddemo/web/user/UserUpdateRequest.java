package com.carddemo.web.user;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Size;

/** COUSR2A editable fields with PF5, plus the version of the user shown (ADR-0010). */
@Schema(description = "COUSR02C update user (PF5): all four fields are required")
public record UserUpdateRequest(
        @Schema(description = "FNAME", example = "JANE", maxLength = 20)
        @Size(max = 20, message = "First Name can be at most 20 characters...") String firstName,
        @Schema(description = "LNAME", example = "DOE", maxLength = 20)
        @Size(max = 20, message = "Last Name can be at most 20 characters...") String lastName,
        @Schema(description = "PASSWD", example = "PASSWORD", maxLength = 8)
        @Size(max = 8, message = "Password can be at most 8 characters...") String password,
        @Schema(description = "USRTYPE: A = admin, U = user", example = "U", maxLength = 1)
        @Size(max = 1, message = "User Type must be A (Admin) or U (User)...") String userType,
        @Schema(description = "version from GET /api/v1/users/{id}; a stale version answers 409 CHANGED",
                example = "0") Long version) {
}
