package com.carddemo.common.error;

/**
 * A screen edit that failed on one field: the COBOL programs check the fields in a fixed order, move
 * the first failure's text to the message line, put the cursor on that field and send the screen back.
 * Answered with 400, the COBOL text as {@code message} and the field as the single {@code fieldErrors}
 * entry.
 */
public class FieldValidationException extends RuntimeException {

    private final String field;

    public FieldValidationException(String field, String message) {
        super(message);
        this.field = field;
    }

    public String field() {
        return field;
    }
}
