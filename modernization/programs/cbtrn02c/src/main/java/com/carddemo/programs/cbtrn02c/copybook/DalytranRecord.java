package com.carddemo.programs.cbtrn02c.copybook;

import java.math.BigDecimal;

/** CVTRA06Y: DALYTRAN-RECORD, daily transaction record (RECLN = 350). */
public record DalytranRecord(
        String dalytranId,           // DALYTRAN-ID            PIC X(16)
        String dalytranTypeCd,       // DALYTRAN-TYPE-CD       PIC X(02)
        int dalytranCatCd,           // DALYTRAN-CAT-CD        PIC 9(04)
        String dalytranSource,       // DALYTRAN-SOURCE        PIC X(10)
        String dalytranDesc,         // DALYTRAN-DESC          PIC X(100)
        BigDecimal dalytranAmt,      // DALYTRAN-AMT           PIC S9(09)V99
        long dalytranMerchantId,     // DALYTRAN-MERCHANT-ID   PIC 9(09)
        String dalytranMerchantName, // DALYTRAN-MERCHANT-NAME PIC X(50)
        String dalytranMerchantCity, // DALYTRAN-MERCHANT-CITY PIC X(50)
        String dalytranMerchantZip,  // DALYTRAN-MERCHANT-ZIP  PIC X(10)
        String dalytranCardNum,      // DALYTRAN-CARD-NUM      PIC X(16)
        String dalytranOrigTs,       // DALYTRAN-ORIG-TS       PIC X(26)
        String dalytranProcTs,       // DALYTRAN-PROC-TS       PIC X(26)
        String filler) {             // FILLER                 PIC X(20)

    public static final int RECORD_LENGTH = 350;
    public static final int AMT_INT_DIGITS = 9;
    public static final int AMT_SCALE = 2;

    public static DalytranRecord parse(String raw) {
        Pic.Reader r = new Pic.Reader(raw, RECORD_LENGTH);
        return new DalytranRecord(r.x(16), r.x(2), (int) r.n(4), r.x(10), r.x(100), r.s(9, 2), r.n(9),
                r.x(50), r.x(50), r.x(10), r.x(16), r.x(26), r.x(26), r.x(20));
    }

    public String format() {
        return new Pic.Writer().x(dalytranId, 16).x(dalytranTypeCd, 2).n(dalytranCatCd, 4)
                .x(dalytranSource, 10).x(dalytranDesc, 100).s(dalytranAmt, 9, 2).n(dalytranMerchantId, 9)
                .x(dalytranMerchantName, 50).x(dalytranMerchantCity, 50).x(dalytranMerchantZip, 10)
                .x(dalytranCardNum, 16).x(dalytranOrigTs, 26).x(dalytranProcTs, 26).x(filler, 20)
                .build(RECORD_LENGTH);
    }
}
