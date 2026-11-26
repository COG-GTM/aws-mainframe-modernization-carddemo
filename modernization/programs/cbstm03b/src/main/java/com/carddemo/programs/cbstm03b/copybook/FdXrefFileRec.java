package com.carddemo.programs.cbstm03b.copybook;

/**
 * FD XREF-FILE / 01 FD-XREFFILE-REC (50 bytes, KSDS key FD-XREF-CARD-NUM).
 * <pre>
 * 05 FD-XREF-CARD-NUM  PIC X(16).
 * 05 FD-XREF-DATA      PIC X(34).
 * </pre>
 */
public record FdXrefFileRec(String fdXrefCardNum, String fdXrefData) {

    public static final int RECORD_LENGTH = 50;
    public static final int KEY_OFFSET = 0;
    public static final int KEY_LENGTH = 16;
    public static final int FD_XREF_DATA_LEN = 34;

    public FdXrefFileRec {
        fdXrefCardNum = Pic.alphanumeric(fdXrefCardNum, KEY_LENGTH);
        fdXrefData = Pic.alphanumeric(fdXrefData, FD_XREF_DATA_LEN);
    }

    public static FdXrefFileRec parse(String record) {
        Pic.requireLength(record, RECORD_LENGTH, "FD-XREFFILE-REC");
        return new FdXrefFileRec(Pic.field(record, 1, 16), Pic.field(record, 17, 34));
    }

    public String toRecord() {
        return fdXrefCardNum + fdXrefData;
    }
}
