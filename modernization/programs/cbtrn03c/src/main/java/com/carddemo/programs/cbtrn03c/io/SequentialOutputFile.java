package com.carddemo.programs.cbtrn03c.io;

/** ORGANIZATION SEQUENTIAL file opened OUTPUT. Each operation returns the FILE STATUS. */
public interface SequentialOutputFile {
    String open();

    String write(String record);

    String close();
}
