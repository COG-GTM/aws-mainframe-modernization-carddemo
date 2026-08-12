package com.carddemo.interest.io;

/**
 * Raised when a fixed-length mainframe record cannot be decoded with its copybook layout.
 *
 * <p>Replaces the COBOL practice of inspecting a two-character {@code FILE STATUS} field and
 * calling {@code CEE3ABD} (see {@code app/cbl/CBACT04C.cbl:628-632}).
 */
public class RecordDecodingException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    public RecordDecodingException(String message) {
        super(message);
    }
}
