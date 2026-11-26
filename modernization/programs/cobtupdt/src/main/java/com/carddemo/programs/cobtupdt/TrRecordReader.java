package com.carddemo.programs.cobtupdt;

import java.util.Optional;

/** Pluggable reader for INPFILE (SELECT TR-RECORD ... ORGANIZATION IS SEQUENTIAL). */
public interface TrRecordReader {

    String STATUS_OK = "00";
    String STATUS_AT_END = "10";
    String STATUS_FILE_NOT_FOUND = "35";
    String STATUS_NOT_OPEN = "47";

    /** OPEN INPUT; returns the two-character FILE STATUS. */
    String open();

    /**
     * READ NEXT RECORD. Returns the raw record, or empty at end of file (AT END).
     *
     * @throws CobolFileStatusException when the read fails (e.g. status 47, file not open)
     */
    Optional<String> readNext();

    /** CLOSE; returns the two-character FILE STATUS. */
    String close();
}
