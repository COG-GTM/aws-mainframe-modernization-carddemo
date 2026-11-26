package com.carddemo.programs.cbtrn03c.copybook;

import com.carddemo.programs.cbtrn03c.support.Fixed;

/**
 * CVTRA03Y - 01 TRAN-TYPE-RECORD (RECLN 60), key TRAN-TYPE.
 *
 * <pre>
 * 05 TRAN-TYPE      PIC X(02)
 * 05 TRAN-TYPE-DESC PIC X(50)
 * 05 FILLER         PIC X(08)
 * </pre>
 */
public record TranTypeRecord(String tranType, String tranTypeDesc, String filler) {

    public static final int LENGTH = 60;
    public static final int KEY_OFFSET = 0;
    public static final int KEY_LENGTH = 2;

    public TranTypeRecord {
        tranType = Fixed.pic(tranType, 2);
        tranTypeDesc = Fixed.pic(tranTypeDesc, 50);
        filler = Fixed.pic(filler, 8);
    }

    public static TranTypeRecord initial() {
        return new TranTypeRecord("", "", "");
    }

    public static TranTypeRecord parse(String record) {
        String r = Fixed.pic(record, LENGTH);
        return new TranTypeRecord(r.substring(0, 2), r.substring(2, 52), r.substring(52, 60));
    }

    public String toFixedWidth() {
        return tranType + tranTypeDesc + filler;
    }
}
