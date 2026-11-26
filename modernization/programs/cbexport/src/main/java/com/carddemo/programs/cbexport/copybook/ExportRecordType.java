package com.carddemo.programs.cbexport.copybook;

/** EXPORT-REC-TYPE PIC X(1) values written by CBEXPORT and read back by CBIMPORT. */
public enum ExportRecordType {
    CUSTOMER('C'),
    ACCOUNT('A'),
    XREF('X'),
    TRANSACTION('T'),
    CARD('D');

    private final char code;

    ExportRecordType(char code) {
        this.code = code;
    }

    public char code() {
        return code;
    }

    /** Returns the type for a code, or {@code null} for any other value (CBIMPORT's "unknown" path). */
    public static ExportRecordType fromCode(char code) {
        for (ExportRecordType t : values()) {
            if (t.code == code) {
                return t;
            }
        }
        return null;
    }
}
