package com.carddemo.programs.cbimport.io;

/**
 * EXPORT-INPUT: indexed file read with ACCESS MODE SEQUENTIAL, i.e. records in ascending
 * EXPORT-SEQUENCE-NUM order. Every operation returns the two-character file status.
 */
public interface ExportRecordReader {

    String open();

    ReadResult read();

    String close();

    /** Status plus the 500-byte record when the status is '00'. */
    record ReadResult(String status, byte[] record) {
    }
}
