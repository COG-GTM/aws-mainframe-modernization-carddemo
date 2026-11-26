package com.carddemo.programs.cbtrn02c.io;

/** Result of a READ: the FILE STATUS and, when successful, the fixed-width record area. */
public record ReadResult(String status, String record) {

    public static ReadResult of(String status) {
        return new ReadResult(status, null);
    }

    public boolean ok() {
        return FileStatus.OK.equals(status);
    }
}
