package com.carddemo.programs.cbact01c.io;

/** Sequential output file opened {@code OUTPUT}. Every operation returns the COBOL FILE STATUS. */
public interface RecordWriter {
    String open();

    String write(byte[] record);

    String close();
}
