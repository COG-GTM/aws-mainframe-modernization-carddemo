package com.carddemo.batch.io;

/** How a variable-length (RECFM=V/VB) record is framed on disk. */
public enum RecordPrefix {
    /** Payload only; what COBOL on z/OS sees once the access method has stripped the RDW. */
    NONE,
    /** GnuCOBOL {@code COB_VARSEQ_FORMAT=1}: 4-byte big-endian payload length, then the payload (the golden files). */
    GNUCOBOL_VARSEQ,
    /** z/OS RDW: 2-byte big-endian length including the 4-byte RDW itself, 2 zero bytes, then the payload. */
    ZOS_RDW
}
