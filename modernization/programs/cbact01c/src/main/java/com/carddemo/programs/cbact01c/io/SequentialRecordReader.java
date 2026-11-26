package com.carddemo.programs.cbact01c.io;

/**
 * Input file opened {@code INPUT} and read with {@code ACCESS MODE IS SEQUENTIAL}.
 * Every operation returns the COBOL FILE STATUS instead of throwing.
 */
public interface SequentialRecordReader {
    String open();

    ReadResult read();

    String close();
}
