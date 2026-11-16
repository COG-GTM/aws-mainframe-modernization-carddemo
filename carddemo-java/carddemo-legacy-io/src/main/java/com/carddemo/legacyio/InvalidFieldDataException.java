package com.carddemo.legacyio;

/** A numeric field was read as a number but its bytes do not hold one. */
public class InvalidFieldDataException extends RuntimeException {

    public InvalidFieldDataException(String message) {
        super(message);
    }
}
