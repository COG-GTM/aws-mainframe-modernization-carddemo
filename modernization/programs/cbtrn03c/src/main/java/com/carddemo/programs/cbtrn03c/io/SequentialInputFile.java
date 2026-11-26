package com.carddemo.programs.cbtrn03c.io;

/** ORGANIZATION SEQUENTIAL file opened INPUT. Each operation returns the FILE STATUS. */
public interface SequentialInputFile {
    String open();

    ReadResult read();

    String close();
}
