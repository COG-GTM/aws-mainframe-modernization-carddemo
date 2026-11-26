package com.carddemo.programs.cbtrn02c.io;

/** ORGANIZATION SEQUENTIAL file opened INPUT. Records are fixed-width strings. */
public interface SequentialReader {
    String open();

    ReadResult read();

    String close();
}
