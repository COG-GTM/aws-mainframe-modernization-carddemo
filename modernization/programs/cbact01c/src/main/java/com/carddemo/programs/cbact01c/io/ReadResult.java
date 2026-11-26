package com.carddemo.programs.cbact01c.io;

/** Outcome of a READ: the FILE STATUS and, when status is "00", the record bytes. */
public record ReadResult(String status, byte[] record) {
    public static ReadResult ok(byte[] record) {
        return new ReadResult(FileStatus.OK, record);
    }

    public static ReadResult status(String status) {
        return new ReadResult(status, null);
    }
}
