package com.carddemo.programs.cbtrn03c.io;

/** Physical layout of a fixed-width data set on disk. */
public enum RecordFormat {
    /** One record per line (LF or CRLF), as in app/data/ASCII. Short lines are space-padded to LRECL. */
    LINE,
    /** RECFM=FB with no delimiters: consecutive LRECL-byte records. */
    FIXED
}
