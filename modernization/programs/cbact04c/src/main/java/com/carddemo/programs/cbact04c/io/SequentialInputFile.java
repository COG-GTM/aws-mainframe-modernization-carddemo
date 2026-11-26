package com.carddemo.programs.cbact04c.io;

/** ORGANIZATION SEQUENTIAL (or INDEXED with ACCESS SEQUENTIAL) file opened INPUT. Methods return FILE STATUS. */
public interface SequentialInputFile<T> {

    String open();

    ReadResult<T> read();

    String close();
}
