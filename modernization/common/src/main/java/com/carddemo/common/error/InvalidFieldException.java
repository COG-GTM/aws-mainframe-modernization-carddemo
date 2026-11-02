package com.carddemo.common.error;

/**
 * A screen edit that needs the stored record, so it cannot be a bean-validation constraint on the
 * request. Answered 400 with the field and message in {@code fieldErrors}, like the other edits.
 */
public class InvalidFieldException extends RuntimeException {

    private final String field;

    public InvalidFieldException(String field, String message) {
        super(message);
        this.field = field;
    }

    public String getField() {
        return field;
    }
}
