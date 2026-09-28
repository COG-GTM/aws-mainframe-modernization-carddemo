package com.carddemo.common;

import java.util.ArrayList;
import java.util.List;

/**
 * Collects field errors in legacy edit order. The legacy screens show only the first error, which becomes the
 * envelope {@code message}; all errors are returned in {@code fieldErrors}.
 */
public class ValidationErrors {

    private final String legacyProgram;
    private final List<FieldErrorDto> errors = new ArrayList<>();

    public ValidationErrors(String legacyProgram) {
        this.legacyProgram = legacyProgram;
    }

    public void add(String field, String message) {
        errors.add(new FieldErrorDto(field, message));
    }

    public boolean hasErrors() {
        return !errors.isEmpty();
    }

    public boolean hasErrorFor(String field) {
        return errors.stream().anyMatch(e -> e.field().equals(field));
    }

    public void throwIfAny() {
        if (hasErrors()) {
            throw new ApiException(ErrorCode.VALIDATION_ERROR, errors.getFirst().message(), legacyProgram, errors);
        }
    }
}
