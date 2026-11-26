package com.carddemo.programs.cbact02c.io;

/**
 * CARDFILE-FILE: ORGANIZATION INDEXED, ACCESS MODE SEQUENTIAL, RECORD KEY FD-CARD-NUM,
 * opened INPUT. Every operation reports a two-byte FILE STATUS (CARDFILE-STATUS) instead of
 * throwing, so the program can apply the COBOL status checks unchanged.
 */
public interface CardFile {

    /** OPEN INPUT CARDFILE-FILE. */
    String open();

    /** READ CARDFILE-FILE (next record in ascending FD-CARD-NUM order). */
    ReadResult read();

    /** CLOSE CARDFILE-FILE. */
    String close();

    /** Status plus the FD-CARDFILE-REC image; record is null when nothing was transferred. */
    record ReadResult(String status, String record) {
    }
}
