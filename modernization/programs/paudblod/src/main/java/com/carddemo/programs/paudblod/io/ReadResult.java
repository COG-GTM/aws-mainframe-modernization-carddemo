package com.carddemo.programs.paudblod.io;

/** Result of a sequential READ: the FILE STATUS and the record area (null when no record). */
public record ReadResult(String status, byte[] record) {

    public static ReadResult ok(byte[] record) {
        return new ReadResult(FileStatus.SUCCESS, record);
    }

    public static ReadResult endOfFile() {
        return new ReadResult(FileStatus.END_OF_FILE, null);
    }

    public static ReadResult error(String status) {
        return new ReadResult(status, null);
    }
}
