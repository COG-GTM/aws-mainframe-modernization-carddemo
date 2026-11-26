package com.carddemo.programs.cbexport.io;

/** Sequential input file (SELECT ... ACCESS MODE IS SEQUENTIAL) reporting COBOL FILE STATUS codes. */
public interface RecordReader extends AutoCloseable {

    /** OPEN INPUT; returns the FILE STATUS. */
    String open();

    /** READ next record. */
    ReadResult read();

    /** CLOSE. */
    @Override
    void close();
}
