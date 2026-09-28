package com.carddemo.common;

import java.time.Instant;
import java.util.List;

public record ErrorResponse(
        ErrorCode errorCode,
        String message,
        List<FieldErrorDto> fieldErrors,
        String legacyProgram,
        Instant timestamp) {

    public static ErrorResponse of(ErrorCode code, String message, List<FieldErrorDto> fieldErrors, String legacyProgram) {
        return new ErrorResponse(code, message, fieldErrors == null ? List.of() : List.copyOf(fieldErrors),
                legacyProgram, Instant.now());
    }
}
