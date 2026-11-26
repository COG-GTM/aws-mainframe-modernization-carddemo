package com.carddemo.programs.cbexport.io;

/**
 * Outcome of a sequential READ: the two-character COBOL FILE STATUS plus the record image
 * ({@code null} unless the status is successful).
 */
public record ReadResult(String status, byte[] record) {

    public static ReadResult ok(byte[] record) {
        return new ReadResult(FileStatus.OK, record);
    }

    public static ReadResult status(String status) {
        return new ReadResult(status, null);
    }
}
