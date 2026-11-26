package com.carddemo.programs.cobtupdt;

import com.carddemo.programs.cobtupdt.copybook.Pic;

/**
 * Input record of INPFILE: FD TR-RECORD (01 WS-INPUT-VARS) and WORKING-STORAGE 01 WS-INPUT-REC
 * share the same 53-byte layout.
 *
 * <pre>
 * 01 WS-INPUT-REC.
 *    05 INPUT-REC-TYPE    PIC X(1).    column 1     A/U/D/*
 *    05 INPUT-REC-NUMBER  PIC X(2).    columns 2-3  transaction type
 *    05 INPUT-REC-DESC    PIC X(50).   columns 4-53 transaction description
 * </pre>
 */
public record TrRecord(String type, String number, String desc) {

    public static final int TYPE_LENGTH = 1;
    public static final int NUMBER_LENGTH = 2;
    public static final int DESC_LENGTH = 50;
    public static final int RECORD_LENGTH = TYPE_LENGTH + NUMBER_LENGTH + DESC_LENGTH;

    /** WS-INPUT-REC VALUE SPACES. */
    public static final TrRecord SPACES = new TrRecord("", "", "");

    public TrRecord {
        type = Pic.alphanumeric(type, TYPE_LENGTH);
        number = Pic.alphanumeric(number, NUMBER_LENGTH);
        desc = Pic.alphanumeric(desc, DESC_LENGTH);
    }

    /** READ ... INTO WS-INPUT-REC: the 53-byte record is moved as a group (padded/truncated to 53). */
    public static TrRecord parse(String rawRecord) {
        String r = Pic.alphanumeric(rawRecord, RECORD_LENGTH);
        return new TrRecord(
                r.substring(0, 1),
                r.substring(1, 3),
                r.substring(3, RECORD_LENGTH));
    }

    public String toFixedWidth() {
        return type + number + desc;
    }

    @Override
    public String toString() {
        return toFixedWidth();
    }
}
