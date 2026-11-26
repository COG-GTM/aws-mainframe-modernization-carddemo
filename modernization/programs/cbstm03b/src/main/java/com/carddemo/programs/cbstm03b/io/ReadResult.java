package com.carddemo.programs.cbstm03b.io;

import com.carddemo.programs.cbstm03b.copybook.FileStatus;

/** Outcome of a READ: the file status and, when status key 1 is '0', the record area. */
public record ReadResult(FileStatus status, String record) {

    public static ReadResult of(FileStatus status) {
        return new ReadResult(status, null);
    }
}
