package com.carddemo.common.api;

import java.time.Instant;
import java.util.List;

public record ApiError(Instant timestamp, int status, String error, String message, List<FieldProblem> fieldErrors) {

    public record FieldProblem(String field, String message) {
    }

    public static ApiError of(int status, String error, String message) {
        return new ApiError(Instant.now(), status, error, message, List.of());
    }

    public static ApiError of(int status, String error, String message, List<FieldProblem> fieldErrors) {
        return new ApiError(Instant.now(), status, error, message, fieldErrors);
    }
}
