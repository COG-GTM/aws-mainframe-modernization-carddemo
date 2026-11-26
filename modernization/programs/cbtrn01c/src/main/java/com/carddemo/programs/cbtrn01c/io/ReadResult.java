package com.carddemo.programs.cbtrn01c.io;

/**
 * Outcome of a READ: the FILE STATUS and, when successful, the record read and (when the file
 * has one) the raw record image as stored, before any field decoding.
 */
public record ReadResult<R>(String status, R record, String image) {

    public ReadResult(String status, R record) {
        this(status, record, null);
    }

    public static <R> ReadResult<R> of(String status) {
        return new ReadResult<>(status, null);
    }
}
