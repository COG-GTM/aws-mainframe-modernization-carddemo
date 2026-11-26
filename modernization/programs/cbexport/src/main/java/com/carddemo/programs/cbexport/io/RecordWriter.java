package com.carddemo.programs.cbexport.io;

/** Sequential output file reporting COBOL FILE STATUS codes. */
public interface RecordWriter extends AutoCloseable {

    /** OPEN OUTPUT; returns the FILE STATUS. */
    String open();

    /** WRITE record; returns the FILE STATUS. */
    String write(byte[] record);

    /** CLOSE. */
    @Override
    void close();
}
