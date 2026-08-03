package com.carddemo.interestcalc.file;

/**
 * The outcome of a COBOL {@code READ}: a file status plus, when the status is {@code '00'},
 * the record that was moved into the record area.
 */
public record KeyedRead<T>(String status, T record) {

    public static <T> KeyedRead<T> found(T record) {
        return new KeyedRead<>(FileStatus.OK, record);
    }

    public static <T> KeyedRead<T> notFound() {
        return new KeyedRead<>(FileStatus.NOT_FOUND, null);
    }

    public static <T> KeyedRead<T> endOfFile() {
        return new KeyedRead<>(FileStatus.END_OF_FILE, null);
    }

    public boolean ok() {
        return FileStatus.OK.equals(status);
    }
}
