package com.carddemo.programs.cbtrn01c.io;

/** Outcome of a READ: the FILE STATUS and, when successful, the record read. */
public record ReadResult<R>(String status, R record) {

    public static <R> ReadResult<R> of(String status) {
        return new ReadResult<>(status, null);
    }
}
