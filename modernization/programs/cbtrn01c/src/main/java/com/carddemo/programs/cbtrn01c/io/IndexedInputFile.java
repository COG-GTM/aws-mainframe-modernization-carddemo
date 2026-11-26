package com.carddemo.programs.cbtrn01c.io;

/** ORGANIZATION INDEXED (VSAM KSDS), ACCESS RANDOM, OPEN INPUT. Keys are the raw record-key bytes. */
public interface IndexedInputFile<R> {

    String open();

    ReadResult<R> read(String key);

    String close();
}
