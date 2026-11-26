package com.carddemo.programs.cbtrn01c.io;

/** ORGANIZATION SEQUENTIAL, OPEN INPUT. Every operation reports a two-byte FILE STATUS. */
public interface SequentialInputFile<R> {

    String open();

    ReadResult<R> read();

    String close();
}
