package com.carddemo.programs.cbstm03b.io;

import java.io.IOException;

/** A dataset record does not fit the LRECL; OPEN reports it as status 39, not as an I/O error. */
public final class RecordLengthException extends IOException {
    public RecordLengthException(String message) {
        super(message);
    }
}
