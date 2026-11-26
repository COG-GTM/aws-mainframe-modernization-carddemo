package com.carddemo.programs.cbstm03b.copybook;

/**
 * FD TRNX-FILE / 01 FD-TRNXFILE-REC (350 bytes, KSDS KEYS(32 0)).
 * <pre>
 * 05 FD-TRNXS-ID.
 *    10 FD-TRNX-CARD   PIC X(16).
 *    10 FD-TRNX-ID     PIC X(16).
 * 05 FD-ACCT-DATA      PIC X(318).
 * </pre>
 */
public record FdTrnxFileRec(String fdTrnxCard, String fdTrnxId, String fdAcctData) {

    public static final int RECORD_LENGTH = 350;
    public static final int KEY_OFFSET = 0;
    public static final int KEY_LENGTH = 32;
    public static final int FD_TRNX_CARD_LEN = 16;
    public static final int FD_TRNX_ID_LEN = 16;
    public static final int FD_ACCT_DATA_LEN = 318;

    public FdTrnxFileRec {
        fdTrnxCard = Pic.alphanumeric(fdTrnxCard, FD_TRNX_CARD_LEN);
        fdTrnxId = Pic.alphanumeric(fdTrnxId, FD_TRNX_ID_LEN);
        fdAcctData = Pic.alphanumeric(fdAcctData, FD_ACCT_DATA_LEN);
    }

    /** FD-TRNXS-ID group (record key). */
    public String fdTrnxsId() {
        return fdTrnxCard + fdTrnxId;
    }

    public static FdTrnxFileRec parse(String record) {
        Pic.requireLength(record, RECORD_LENGTH, "FD-TRNXFILE-REC");
        return new FdTrnxFileRec(Pic.field(record, 1, 16), Pic.field(record, 17, 16), Pic.field(record, 33, 318));
    }

    public String toRecord() {
        return fdTrnxCard + fdTrnxId + fdAcctData;
    }
}
