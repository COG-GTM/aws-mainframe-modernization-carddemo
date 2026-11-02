package com.carddemo.auth.api.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

/** COSGN00C signon map fields; the blank-field messages are the COBOL ones. */
public record SignonRequest(
        @NotBlank(message = "Please enter User ID ...") @Size(max = 8) String userId,
        @NotBlank(message = "Please enter Password ...") @Size(max = 64) String password) {
}
