package com.carddemo.legacyio;

/**
 * How one record was stored in a line-sequential file, so it can be written back unchanged. Lines may be shorter than
 * the record (trailing spaces trimmed, as some {@code app/data/ASCII} files are) and end in LF, CRLF or nothing.
 *
 * @param length bytes of the record that were stored on the line
 * @param terminator the line ending that followed them
 */
public record LineFraming(int length, Terminator terminator) {

    /** Line ending of a line-sequential record. */
    public enum Terminator {
        LF(new byte[] {'\n'}),
        CRLF(new byte[] {'\r', '\n'}),
        /** The last line of a file with no final newline. */
        NONE(new byte[0]);

        private final byte[] bytes;

        Terminator(byte[] bytes) {
            this.bytes = bytes;
        }

        byte[] bytes() {
            return bytes.clone();
        }
    }
}
