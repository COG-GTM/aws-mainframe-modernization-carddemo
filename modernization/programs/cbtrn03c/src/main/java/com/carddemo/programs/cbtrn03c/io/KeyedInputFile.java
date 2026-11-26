package com.carddemo.programs.cbtrn03c.io;

/** ORGANIZATION INDEXED, ACCESS MODE RANDOM file (VSAM KSDS) opened INPUT. */
public interface KeyedInputFile {
    String open();

    /** Random READ by RECORD KEY; status '23' when the key does not exist. */
    ReadResult read(String key);

    String close();
}
