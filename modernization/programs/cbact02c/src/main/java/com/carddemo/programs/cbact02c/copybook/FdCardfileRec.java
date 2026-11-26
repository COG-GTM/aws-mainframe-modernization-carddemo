package com.carddemo.programs.cbact02c.copybook;

/**
 * FD record of CARDFILE-FILE in CBACT02C (file section layout, RECLN 150).
 *
 * <pre>
 * 01  FD-CARDFILE-REC.
 *     05 FD-CARD-NUM     PIC X(16).   RECORD KEY
 *     05 FD-CARD-DATA    PIC X(134).
 * </pre>
 */
public record FdCardfileRec(String fdCardNum, String fdCardData) {

    public static final int RECORD_LENGTH = 150;
    public static final int FD_CARD_NUM_LEN = 16;
    public static final int FD_CARD_DATA_LEN = 134;

    public FdCardfileRec {
        fdCardNum = Pic.alpha(fdCardNum, FD_CARD_NUM_LEN);
        fdCardData = Pic.alpha(fdCardData, FD_CARD_DATA_LEN);
    }

    public static FdCardfileRec fromFixedWidth(String image) {
        String rec = Pic.alpha(image, RECORD_LENGTH);
        return new FdCardfileRec(rec.substring(0, FD_CARD_NUM_LEN), rec.substring(FD_CARD_NUM_LEN));
    }

    public String toFixedWidth() {
        return fdCardNum + fdCardData;
    }
}
