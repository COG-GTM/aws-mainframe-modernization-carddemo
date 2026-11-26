package com.carddemo.programs.cbstm03a.io;

/** Output side of a fixed-length (RECFM=FB) sequential file. */
public interface RecordWriter extends AutoCloseable {

    /** WRITE: the record is already padded/truncated to the file's LRECL. */
    void write(String record);

    @Override
    void close();
}
