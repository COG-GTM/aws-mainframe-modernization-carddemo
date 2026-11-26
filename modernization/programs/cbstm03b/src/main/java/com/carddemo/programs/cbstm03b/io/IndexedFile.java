package com.carddemo.programs.cbstm03b.io;

import com.carddemo.programs.cbstm03b.copybook.FileStatus;

/** A VSAM KSDS opened INPUT, as used by CBSTM03B. Every operation returns the COBOL file status. */
public interface IndexedFile {

    /** OPEN INPUT. */
    FileStatus open();

    /** CLOSE. */
    FileStatus close();

    /** READ (ACCESS MODE SEQUENTIAL): next record in ascending key order. */
    ReadResult readNext();

    /** READ (ACCESS MODE RANDOM) with the RECORD KEY already moved to {@code key}. */
    ReadResult readByKey(String key);
}
