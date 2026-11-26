package com.carddemo.programs.cbtrn03c.copybook;

import com.carddemo.programs.cbtrn03c.support.Fixed;

/**
 * CVTRA04Y - 01 TRAN-CAT-RECORD (RECLN 60), key TRAN-CAT-KEY.
 *
 * <pre>
 * 05 TRAN-CAT-KEY.
 *    10 TRAN-TYPE-CD    PIC X(02)
 *    10 TRAN-CAT-CD     PIC 9(04)
 * 05 TRAN-CAT-TYPE-DESC PIC X(50)
 * 05 FILLER             PIC X(04)
 * </pre>
 */
public record TranCatRecord(TranCatKey tranCatKey, String tranCatTypeDesc, String filler) {

    public static final int LENGTH = 60;
    public static final int KEY_OFFSET = 0;
    public static final int KEY_LENGTH = TranCatKey.LENGTH;

    /** 05 TRAN-CAT-KEY group. */
    public record TranCatKey(String tranTypeCd, int tranCatCd) {
        public static final int LENGTH = 6;

        public TranCatKey {
            tranTypeCd = Fixed.pic(tranTypeCd, 2);
        }

        public static TranCatKey parse(String key) {
            String k = Fixed.pic(key, LENGTH);
            return new TranCatKey(k.substring(0, 2), (int) Fixed.parseUnsigned(k.substring(2, 6), "TRAN-CAT-CD"));
        }

        public String toFixedWidth() {
            return tranTypeCd + Fixed.unsigned(tranCatCd, 4);
        }
    }

    public TranCatRecord {
        tranCatKey = tranCatKey == null ? new TranCatKey("", 0) : tranCatKey;
        tranCatTypeDesc = Fixed.pic(tranCatTypeDesc, 50);
        filler = Fixed.pic(filler, 4);
    }

    public static TranCatRecord initial() {
        return new TranCatRecord(new TranCatKey("", 0), "", "");
    }

    public static TranCatRecord parse(String record) {
        String r = Fixed.pic(record, LENGTH);
        return new TranCatRecord(TranCatKey.parse(r.substring(0, 6)), r.substring(6, 56), r.substring(56, 60));
    }

    public String toFixedWidth() {
        return tranCatKey.toFixedWidth() + tranCatTypeDesc + filler;
    }
}
