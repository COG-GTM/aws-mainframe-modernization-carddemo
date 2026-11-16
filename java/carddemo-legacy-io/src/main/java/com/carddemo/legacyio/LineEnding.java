package com.carddemo.legacyio;

/** Terminator that followed a record in a file. */
public enum LineEnding {
    NONE(new byte[0]),
    LF(new byte[] {'\n'}),
    CRLF(new byte[] {'\r', '\n'});

    private final byte[] bytes;

    LineEnding(byte[] bytes) {
        this.bytes = bytes;
    }

    public byte[] bytes() {
        return bytes.clone();
    }
}
