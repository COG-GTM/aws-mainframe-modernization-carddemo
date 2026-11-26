package com.carddemo.programs.cbstm03a.io;

import java.util.Optional;

/** Input side of a sequential (or sequentially accessed KSDS) file. */
public interface RecordReader extends AutoCloseable {

    /** Next record padded to the record length, or empty at end of file. */
    Optional<String> read();

    @Override
    void close();
}
