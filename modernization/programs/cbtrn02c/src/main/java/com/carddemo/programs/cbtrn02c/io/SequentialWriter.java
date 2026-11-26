package com.carddemo.programs.cbtrn02c.io;

/** ORGANIZATION SEQUENTIAL file opened OUTPUT. Records are fixed-width strings. */
public interface SequentialWriter {
    String open();

    String write(String record);

    String close();
}
