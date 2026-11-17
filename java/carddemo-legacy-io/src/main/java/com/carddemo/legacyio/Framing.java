package com.carddemo.legacyio;

/** How records are delimited in a file. */
public enum Framing {
    /** Records back to back, each exactly the record length (z/OS RECFM=F/FB extracts, the EBCDIC samples). */
    FIXED,
    /**
     * One record per line ended by LF or CRLF (GnuCOBOL LINE SEQUENTIAL, the ASCII samples). A line may be shorter
     * than the record: the missing tail reads as spaces.
     */
    LINE_SEQUENTIAL
}
