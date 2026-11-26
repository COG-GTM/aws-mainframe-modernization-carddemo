package com.carddemo.programs.cbstm03b.copybook;

/**
 * FD CUST-FILE / 01 FD-CUSTFILE-REC (500 bytes, KSDS key FD-CUST-ID).
 * <pre>
 * 05 FD-CUST-ID        PIC X(09).
 * 05 FD-CUST-DATA      PIC X(491).
 * </pre>
 */
public record FdCustFileRec(String fdCustId, String fdCustData) {

    public static final int RECORD_LENGTH = 500;
    public static final int KEY_OFFSET = 0;
    public static final int KEY_LENGTH = 9;
    public static final int FD_CUST_DATA_LEN = 491;

    public FdCustFileRec {
        fdCustId = Pic.alphanumeric(fdCustId, KEY_LENGTH);
        fdCustData = Pic.alphanumeric(fdCustData, FD_CUST_DATA_LEN);
    }

    /** MOVE source TO FD-CUST-ID (PIC X(09)). */
    public static String toKey(String source) {
        return Pic.alphanumeric(source, KEY_LENGTH);
    }

    public static FdCustFileRec parse(String record) {
        Pic.requireLength(record, RECORD_LENGTH, "FD-CUSTFILE-REC");
        return new FdCustFileRec(Pic.field(record, 1, 9), Pic.field(record, 10, 491));
    }

    public String toRecord() {
        return fdCustId + fdCustData;
    }
}
