package com.carddemo.programs.cbstm03b.copybook;

/**
 * FD ACCT-FILE / 01 FD-ACCTFILE-REC (300 bytes, KSDS key FD-ACCT-ID).
 * <pre>
 * 05 FD-ACCT-ID        PIC 9(11).
 * 05 FD-ACCT-DATA      PIC X(289).
 * </pre>
 * FD-ACCT-ID is kept as its 11 DISPLAY digits because it is the VSAM key (byte-compared).
 */
public record FdAcctFileRec(String fdAcctId, String fdAcctData) {

    public static final int RECORD_LENGTH = 300;
    public static final int KEY_OFFSET = 0;
    public static final int KEY_LENGTH = 11;
    public static final int FD_ACCT_DATA_LEN = 289;

    public FdAcctFileRec {
        fdAcctId = Pic.unsignedNumericFromAlphanumeric(fdAcctId, KEY_LENGTH);
        fdAcctData = Pic.alphanumeric(fdAcctData, FD_ACCT_DATA_LEN);
    }

    /** MOVE alphanumeric source TO FD-ACCT-ID (PIC 9(11)). */
    public static String toKey(String source) {
        return Pic.unsignedNumericFromAlphanumeric(source, KEY_LENGTH);
    }

    /** FD-ACCT-ID as a number. */
    public long fdAcctIdValue() {
        return Long.parseLong(fdAcctId);
    }

    public static FdAcctFileRec parse(String record) {
        Pic.requireLength(record, RECORD_LENGTH, "FD-ACCTFILE-REC");
        return new FdAcctFileRec(Pic.field(record, 1, 11), Pic.field(record, 12, 289));
    }

    public String toRecord() {
        return fdAcctId + fdAcctData;
    }
}
