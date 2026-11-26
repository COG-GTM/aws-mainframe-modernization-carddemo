package com.carddemo.programs.cbact04c.io;

/** Outcome of a READ: the FILE STATUS plus the record when the status is '00'. */
public record ReadResult<T>(String status, T record) {

    public static <T> ReadResult<T> ok(T record) {
        return new ReadResult<>(FileStatus.OK, record);
    }

    public static <T> ReadResult<T> status(String status) {
        return new ReadResult<>(status, null);
    }
}
