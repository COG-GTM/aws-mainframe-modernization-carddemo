package com.carddemo.common.codec;

/** A record image, copybook or field value that does not match its COBOL definition. */
public class RecordFormatException extends RuntimeException {

    public RecordFormatException(String message) {
        super(message);
    }

    public RecordFormatException(String message, Throwable cause) {
        super(message, cause);
    }
}
