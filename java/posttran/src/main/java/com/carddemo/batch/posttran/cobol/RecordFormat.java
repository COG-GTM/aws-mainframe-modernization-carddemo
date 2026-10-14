package com.carddemo.batch.posttran.cobol;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

/**
 * Physical form of a sequential dataset. Records are handled as Java strings with one char per
 * byte, so ASCII uses ISO-8859-1 (a 1:1 byte mapping) and EBCDIC uses CP037.
 */
public enum RecordFormat {
    /** Line-sequential: one record per line, LF or CRLF, short lines space-padded. */
    ASCII(StandardCharsets.ISO_8859_1, true),
    /** RECFM=F: fixed-length records, no delimiters, CP037. */
    EBCDIC(Charset.forName("IBM037"), false);

    private final Charset charset;
    private final boolean lineDelimited;

    RecordFormat(Charset charset, boolean lineDelimited) {
        this.charset = charset;
        this.lineDelimited = lineDelimited;
    }

    public Charset charset() {
        return charset;
    }

    public boolean lineDelimited() {
        return lineDelimited;
    }
}
