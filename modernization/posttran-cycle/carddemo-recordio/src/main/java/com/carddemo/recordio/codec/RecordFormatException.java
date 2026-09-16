package com.carddemo.recordio.codec;

/** A record image does not conform to its copybook layout (bad length, non-numeric digit, ...). */
public class RecordFormatException extends RuntimeException {

    public RecordFormatException(String message) {
        super(message);
    }
}
