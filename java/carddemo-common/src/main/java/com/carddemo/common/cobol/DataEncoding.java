package com.carddemo.common.cobol;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

/**
 * Character encoding of DISPLAY data in a record. {@link #EBCDIC} is the mainframe code page (IBM-037, the code
 * page of the CardDemo sample datasets); {@link #ASCII} is the single-byte Latin-1 form produced when the
 * datasets are transferred in text mode (sign overpunch preserved as '{', 'A'-'I', '}', 'J'-'R').
 */
public enum DataEncoding {
    EBCDIC(Charset.forName("IBM037")),
    ASCII(StandardCharsets.ISO_8859_1);

    private final Charset charset;
    private final byte space;

    DataEncoding(Charset charset) {
        this.charset = charset;
        this.space = " ".getBytes(charset)[0];
    }

    public Charset charset() {
        return charset;
    }

    public byte space() {
        return space;
    }

    public String decode(byte[] buf, int offset, int length) {
        return new String(buf, offset, length, charset);
    }

    /** Encodes text byte-for-byte; unmappable characters become the code page's substitution character. */
    public byte[] encode(String text) {
        return text.getBytes(charset);
    }
}
