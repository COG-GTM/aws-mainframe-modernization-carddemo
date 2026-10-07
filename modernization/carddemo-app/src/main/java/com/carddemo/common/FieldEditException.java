package com.carddemo.common;

import java.util.List;

/**
 * Map edit errors of a whole screen: {@link #field()} and the message are the first error (the one COBOL puts in
 * {@code WS-RETURN-MSG}); {@link #invalidFields()} are all the fields the program would highlight.
 */
public class FieldEditException extends InvalidRequestException {

    private final List<String> invalidFields;

    public FieldEditException(String field, String message, List<String> invalidFields) {
        super(field, message);
        this.invalidFields = List.copyOf(invalidFields);
    }

    public List<String> invalidFields() {
        return invalidFields;
    }
}
