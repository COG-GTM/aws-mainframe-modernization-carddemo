package com.carddemo.batch.codec;

/** Raised when bytes cannot be decoded with the copybook layout (bad digit, bad sign nibble, short record). */
public class CodecException extends RuntimeException {
    private static final long serialVersionUID = 1L;

    public CodecException(String message) {
        super(message);
    }
}
