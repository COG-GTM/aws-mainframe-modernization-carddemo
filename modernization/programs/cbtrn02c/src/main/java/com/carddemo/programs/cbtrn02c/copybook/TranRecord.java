package com.carddemo.programs.cbtrn02c.copybook;

import java.math.BigDecimal;

/** CVTRA05Y: TRAN-RECORD, posted transaction record (RECLN = 350). Key TRAN-ID. */
public record TranRecord(
        String tranId,           // TRAN-ID            PIC X(16)
        String tranTypeCd,       // TRAN-TYPE-CD       PIC X(02)
        int tranCatCd,           // TRAN-CAT-CD        PIC 9(04)
        String tranSource,       // TRAN-SOURCE        PIC X(10)
        String tranDesc,         // TRAN-DESC          PIC X(100)
        BigDecimal tranAmt,      // TRAN-AMT           PIC S9(09)V99
        long tranMerchantId,     // TRAN-MERCHANT-ID   PIC 9(09)
        String tranMerchantName, // TRAN-MERCHANT-NAME PIC X(50)
        String tranMerchantCity, // TRAN-MERCHANT-CITY PIC X(50)
        String tranMerchantZip,  // TRAN-MERCHANT-ZIP  PIC X(10)
        String tranCardNum,      // TRAN-CARD-NUM      PIC X(16)
        String tranOrigTs,       // TRAN-ORIG-TS       PIC X(26)
        String tranProcTs,       // TRAN-PROC-TS       PIC X(26)
        String filler) {         // FILLER             PIC X(20)

    public static final int RECORD_LENGTH = 350;
    public static final int KEY_OFFSET = 0;
    public static final int KEY_LENGTH = 16;
    public static final int PROC_TS_OFFSET = 304;

    public static TranRecord parse(String raw) {
        Pic.Reader r = new Pic.Reader(raw, RECORD_LENGTH);
        return new TranRecord(r.x(16), r.x(2), (int) r.n(4), r.x(10), r.x(100), r.s(9, 2), r.n(9),
                r.x(50), r.x(50), r.x(10), r.x(16), r.x(26), r.x(26), r.x(20));
    }

    public String format() {
        return new Pic.Writer().x(tranId, 16).x(tranTypeCd, 2).n(tranCatCd, 4).x(tranSource, 10)
                .x(tranDesc, 100).s(tranAmt, 9, 2).n(tranMerchantId, 9).x(tranMerchantName, 50)
                .x(tranMerchantCity, 50).x(tranMerchantZip, 10).x(tranCardNum, 16).x(tranOrigTs, 26)
                .x(tranProcTs, 26).x(filler, 20).build(RECORD_LENGTH);
    }
}
