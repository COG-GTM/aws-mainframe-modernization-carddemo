package com.carddemo.programs.cbtrn03c.io;

/** Outcome of a READ: the file status and, when successful, the record image. */
public record ReadResult(String status, String record) {

    public static ReadResult ok(String record) {
        return new ReadResult(FileStatus.OK, record);
    }

    public static ReadResult status(String status) {
        return new ReadResult(status, null);
    }
}
