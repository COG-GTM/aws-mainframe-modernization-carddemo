package com.carddemo.programs.cbact03c;

/**
 * XREFFILE-FILE: INDEXED, ACCESS MODE SEQUENTIAL, RECORD KEY FD-XREF-CARD-NUM.
 * Every operation returns the two-byte FILE STATUS, exactly as XREFFILE-STATUS would hold it.
 */
public interface XrefFile {

    /** OPEN INPUT. */
    String open();

    /** READ next record in key order. */
    ReadResult readNext();

    /** CLOSE. */
    String close();

    /** Status plus the 50-byte record image (null unless status is "00"). */
    record ReadResult(String status, String record) {
        public static ReadResult of(String record) {
            return new ReadResult(FileStatus.OK, record);
        }

        public static ReadResult status(String status) {
            return new ReadResult(status, null);
        }
    }
}
