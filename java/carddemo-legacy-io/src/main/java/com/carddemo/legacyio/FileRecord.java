package com.carddemo.legacyio;

/**
 * A record as it was stored in a file: its values plus the framing needed to write the same bytes back.
 *
 * @param storedLength bytes of the record present in the file; less than the layout length for a line-sequential
 *                     line whose trailing spaces were omitted
 * @param ending       terminator that followed the record
 */
public record FileRecord(Record record, int storedLength, LineEnding ending) {

    public FileRecord {
        if (storedLength < 0 || storedLength > record.layout().length()) {
            throw new IllegalArgumentException("stored length " + storedLength + " outside " + record.layout());
        }
    }
}
