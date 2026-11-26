package com.carddemo.programs.cbtrn02c.copybook;

import java.math.BigDecimal;

/** CVTRA01Y: TRAN-CAT-BAL-RECORD, transaction category balance (RECLN = 50). Key TRAN-CAT-KEY. */
public record TranCatBalRecord(
        TranCatKey tranCatKey,   // TRAN-CAT-KEY
        BigDecimal tranCatBal,   // TRAN-CAT-BAL PIC S9(09)V99
        String filler) {         // FILLER       PIC X(22)

    public static final int RECORD_LENGTH = 50;
    public static final int KEY_OFFSET = 0;
    public static final int KEY_LENGTH = TranCatKey.LENGTH;
    public static final int BAL_INT_DIGITS = 9;
    public static final int BAL_SCALE = 2;

    /** TRAN-CAT-KEY group: TRANCAT-ACCT-ID 9(11), TRANCAT-TYPE-CD X(02), TRANCAT-CD 9(04). */
    public record TranCatKey(long trancatAcctId, String trancatTypeCd, int trancatCd) {
        public static final int LENGTH = 17;

        public String format() {
            return Pic.unsigned(trancatAcctId, 11) + Pic.alnum(trancatTypeCd, 2) + Pic.unsigned(trancatCd, 4);
        }
    }

    public static TranCatBalRecord parse(String raw) {
        Pic.Reader r = new Pic.Reader(raw, RECORD_LENGTH);
        return new TranCatBalRecord(new TranCatKey(r.n(11), r.x(2), (int) r.n(4)), r.s(9, 2), r.x(22));
    }

    public String format() {
        return new Pic.Writer().x(tranCatKey.format(), TranCatKey.LENGTH).s(tranCatBal, 9, 2).x(filler, 22)
                .build(RECORD_LENGTH);
    }
}
