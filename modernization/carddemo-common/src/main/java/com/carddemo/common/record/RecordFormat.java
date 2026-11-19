package com.carddemo.common.record;

/** Physical organization of a sequential record file. */
public enum RecordFormat {
    /**
     * One record per line ({@code app/data/ASCII}): {@code \n} or {@code \r\n} terminated, short lines
     * right-padded with blanks to the record length (cardxref.txt is 36 of 50 bytes, tcatbal.txt has CRLF).
     */
    LINE_SEQUENTIAL,
    /** Fixed-length records with no separator (RECFM=FB, e.g. {@code app/data/EBCDIC/*.PS}, DALYREJS). */
    FIXED_BLOCK
}
