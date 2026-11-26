package com.carddemo.programs.paudblod.io;

/** Pluggable input for an {@code ORGANIZATION IS SEQUENTIAL} file opened INPUT. */
public interface SequentialRecordReader {

    /** OPEN INPUT; returns the FILE STATUS. */
    String open();

    /** READ next record. */
    ReadResult read();

    /** CLOSE; returns the FILE STATUS. */
    String close();
}
