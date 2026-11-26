package com.carddemo.programs.paudbunl.io;

import java.util.Optional;

/** Sequential input file of fixed-length records. */
public interface RecordReader extends AutoCloseable {

    int recordLength();

    /** READ ... AT END: empty when the file is exhausted (FILE STATUS '10'). */
    Optional<byte[]> read();

    @Override
    void close();
}
