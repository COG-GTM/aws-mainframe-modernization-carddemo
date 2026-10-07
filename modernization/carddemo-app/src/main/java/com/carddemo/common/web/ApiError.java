package com.carddemo.common.web;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Uniform error body of every online endpoint (ADR-0019): an RFC 7807 problem whose {@code code}, {@code field} and
 * {@code message} members carry what the COBOL program put on the screen. Built with {@link ApiErrors}; this record
 * documents the shape in OpenAPI.
 */
@Schema(name = "ApiError", description = "Uniform error body: RFC 7807 problem + code/field/message (ADR-0019)")
public record ApiError(
        @Schema(description = "Stable error code: the CICS condition (NOTFND, INVREQ, NOTAUTH, DUPREC, ...) or a "
                + "program-specific reason", example = "NOTFND") String code,
        @Schema(description = "Request field the error refers to (the BMS field the COBOL put the cursor on); null "
                + "when the error is not about one field", example = "userId", nullable = true) String field,
        @Schema(description = "Message exactly as the COBOL program shows it in ERRMSG (right-trimmed)",
                example = "User not found. Try again ...") String message,
        @Schema(description = "HTTP status", example = "401") int status,
        @Schema(description = "HTTP reason phrase", example = "Unauthorized") String title,
        @Schema(description = "Same text as message", example = "User not found. Try again ...") String detail,
        @Schema(description = "Request path", example = "/api/v1/auth/login") String instance) {
}
