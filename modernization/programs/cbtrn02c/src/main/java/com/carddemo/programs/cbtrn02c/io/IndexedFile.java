package com.carddemo.programs.cbtrn02c.io;

/** ORGANIZATION INDEXED (VSAM KSDS), ACCESS MODE RANDOM. Records are fixed-width strings. */
public interface IndexedFile {
    String open(OpenMode mode);

    /** READ with RECORD KEY set to {@code key}. */
    ReadResult read(String key);

    String write(String record);

    String rewrite(String record);

    String close();
}
