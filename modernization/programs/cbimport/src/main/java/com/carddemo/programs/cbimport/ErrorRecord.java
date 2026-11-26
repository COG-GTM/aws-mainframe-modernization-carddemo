package com.carddemo.programs.cbimport;

import com.carddemo.programs.cbimport.copybook.Pic;

/**
 * WS-ERROR-RECORD (130 bytes), written to ERROUT (LRECL=132) via WRITE ... FROM, which pads the
 * two extra bytes with spaces.
 */
public record ErrorRecord(
        String timestamp,               // ERR-TIMESTAMP X(26)
        String recordType,              // ERR-RECORD-TYPE X(01)
        long sequence,                  // ERR-SEQUENCE 9(07)
        String message) {               // ERR-MESSAGE X(50)

    public static final int WS_LENGTH = 130;
    public static final int OUTPUT_LENGTH = 132;

    public ErrorRecord {
        timestamp = Pic.alnum(timestamp, 26);
        recordType = Pic.alnum(recordType, 1);
        sequence = Pic.unsigned(sequence, 7);
        message = Pic.alnum(message, 50);
    }

    public String toWorkingStorage() {
        return timestamp + "|" + recordType + "|" + Pic.displayUnsigned(sequence, 7) + "|" + message
                + " ".repeat(43);
    }

    public String toOutputRecord() {
        return Pic.alnum(toWorkingStorage(), OUTPUT_LENGTH);
    }
}
