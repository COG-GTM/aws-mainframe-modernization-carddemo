package com.carddemo.auth.api.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

/** COSGN00C signon map fields. */
public record SignonRequest(
        @NotBlank @Size(max = 8) String userId,
        @NotBlank @Size(max = 64) String password) {
}
