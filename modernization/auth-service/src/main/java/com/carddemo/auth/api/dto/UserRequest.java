package com.carddemo.auth.api.dto;

import jakarta.validation.constraints.Size;

/**
 * COUSR01C add user / COUSR02C update user payload: the COUSR1A/COUSR2A map fields. The required-field
 * and user-type edits run in {@link com.carddemo.auth.service.UserEdits} in the COBOL order so the
 * first failing field answers with the COBOL text; only the lengths are Bean Validation here. On update
 * {@code userId} may be omitted (the path carries it).
 */
public record UserRequest(
        @Size(max = 8) String userId,
        @Size(max = 20) String firstName,
        @Size(max = 20) String lastName,
        @Size(max = 64) String password,
        String userType) {
}
