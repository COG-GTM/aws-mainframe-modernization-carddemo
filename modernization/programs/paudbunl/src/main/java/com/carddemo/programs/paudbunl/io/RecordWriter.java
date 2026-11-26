package com.carddemo.programs.paudbunl.io;

/** Sequential output file (FD + SELECT ... ORGANIZATION SEQUENTIAL). Every call returns a FILE STATUS. */
public interface RecordWriter {

    /** Fixed record length (LRECL, RECFM=FB). */
    int recordLength();

    /** OPEN OUTPUT. */
    String open();

    /** WRITE record. */
    String write(byte[] record);

    /** CLOSE. */
    String close();

    boolean isOpen();
}
