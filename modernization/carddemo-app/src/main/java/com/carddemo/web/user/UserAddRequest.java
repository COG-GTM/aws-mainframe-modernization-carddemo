package com.carddemo.web.user;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Size;

/** COUSR1A input fields. Blank fields are rejected in screen order by the service (COUSR01C R-8..R-12). */
@Schema(description = "COUSR01C add user: all five fields are required")
public record UserAddRequest(
        @Schema(description = "FNAME", example = "JANE", maxLength = 20)
        @Size(max = 20, message = "First Name can be at most 20 characters...") String firstName,
        @Schema(description = "LNAME", example = "DOE", maxLength = 20)
        @Size(max = 20, message = "Last Name can be at most 20 characters...") String lastName,
        @Schema(description = "USERID (SEC-USR-ID, up to 8 characters)", example = "JDOE0001", maxLength = 8)
        @Size(max = 8, message = "User ID can be at most 8 characters...") String userId,
        @Schema(description = "PASSWD (stored in plaintext like USRSEC, ADR-0018)", example = "PASSWORD",
                maxLength = 8)
        @Size(max = 8, message = "Password can be at most 8 characters...") String password,
        @Schema(description = "USRTYPE: A = admin, U = user", example = "U", maxLength = 1)
        @Size(max = 1, message = "User Type must be A (Admin) or U (User)...") String userType) {
}
