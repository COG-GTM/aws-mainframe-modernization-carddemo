package com.carddemo.auth.api.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;

/** COUSR01C add user / COUSR02C update user payload. */
public record UserRequest(
        @NotBlank @Size(max = 8) String userId,
        @Size(max = 20) String firstName,
        @Size(max = 20) String lastName,
        @NotBlank @Size(min = 8, max = 64) String password,
        @NotBlank @Pattern(regexp = "[AU]", message = "user type must be A or U") String userType) {
}
