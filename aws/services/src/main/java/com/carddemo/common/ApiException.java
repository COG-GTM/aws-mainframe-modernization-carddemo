package com.carddemo.common;

import java.util.List;

public class ApiException extends RuntimeException {

    private final ErrorCode errorCode;
    private final String legacyProgram;
    private final List<FieldErrorDto> fieldErrors;

    public ApiException(ErrorCode errorCode, String message, String legacyProgram, List<FieldErrorDto> fieldErrors) {
        super(message);
        this.errorCode = errorCode;
        this.legacyProgram = legacyProgram;
        this.fieldErrors = fieldErrors == null ? List.of() : List.copyOf(fieldErrors);
    }

    public ApiException(ErrorCode errorCode, String message, String legacyProgram) {
        this(errorCode, message, legacyProgram, List.of());
    }

    public static ApiException validation(String legacyProgram, String field, String message) {
        return new ApiException(ErrorCode.VALIDATION_ERROR, message, legacyProgram,
                List.of(new FieldErrorDto(field, message)));
    }

    public static ApiException notFound(String legacyProgram, String message) {
        return new ApiException(ErrorCode.NOT_FOUND, message, legacyProgram);
    }

    public static ApiException businessRule(String legacyProgram, String message) {
        return new ApiException(ErrorCode.BUSINESS_RULE, message, legacyProgram);
    }

    public static ApiException concurrentUpdate(String legacyProgram) {
        return new ApiException(ErrorCode.CONCURRENT_UPDATE, LegacyMessages.RECORD_CHANGED, legacyProgram);
    }

    public ErrorCode errorCode() {
        return errorCode;
    }

    public String legacyProgram() {
        return legacyProgram;
    }

    public List<FieldErrorDto> fieldErrors() {
        return fieldErrors;
    }
}
