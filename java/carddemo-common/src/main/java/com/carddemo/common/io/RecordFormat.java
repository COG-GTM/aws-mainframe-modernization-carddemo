package com.carddemo.common.io;

/** Physical record format of a dataset. */
public enum RecordFormat {
    /** RECFM=F/FB: back-to-back records of exactly LRECL bytes (binary transfer of a PS or VSAM REPRO). */
    FIXED,
    /** RECFM=V/VB: each record prefixed by a 4-byte Record Descriptor Word (big-endian length incl. the RDW). */
    VARIABLE,
    /** Text transfer: one record per LF/CRLF terminated line, right-padded with spaces to LRECL. */
    LINE
}
