package com.carddemo.migration;

public enum SourceEncoding {
    /** Fixed-length EBCDIC records exactly as unloaded from VSAM via IDCAMS REPRO (RECFM=FB, no delimiters). */
    EBCDIC,
    /** Line-delimited ASCII records (app/data/ASCII) produced by a text-mode transfer; short lines are space padded. */
    ASCII
}
