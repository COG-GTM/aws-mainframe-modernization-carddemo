package com.carddemo.programs.cotrn00c.copybook;

import java.math.BigDecimal;

/**
 * CVTRA05Y - TRAN-RECORD, the TRANSACT VSAM KSDS record (RECLN 350, key TRAN-ID).
 * The trailing FILLER X(20) is emitted as spaces on serialisation.
 */
public record TranRecord(
        String tranId,            // TRAN-ID            PIC X(16)
        String tranTypeCd,        // TRAN-TYPE-CD       PIC X(02)
        int tranCatCd,            // TRAN-CAT-CD        PIC 9(04)
        String tranSource,        // TRAN-SOURCE        PIC X(10)
        String tranDesc,          // TRAN-DESC          PIC X(100)
        BigDecimal tranAmt,       // TRAN-AMT           PIC S9(09)V99
        long tranMerchantId,      // TRAN-MERCHANT-ID   PIC 9(09)
        String tranMerchantName,  // TRAN-MERCHANT-NAME PIC X(50)
        String tranMerchantCity,  // TRAN-MERCHANT-CITY PIC X(50)
        String tranMerchantZip,   // TRAN-MERCHANT-ZIP  PIC X(10)
        String tranCardNum,       // TRAN-CARD-NUM      PIC X(16)
        String tranOrigTs,        // TRAN-ORIG-TS       PIC X(26)
        String tranProcTs) {      // TRAN-PROC-TS       PIC X(26)

    public static final int RECORD_LENGTH = 350;
    public static final int TRAN_ID_LENGTH = 16;
    public static final int TRAN_DESC_LENGTH = 100;
    public static final int TRAN_AMT_DIGITS = 11;
    public static final int TRAN_AMT_SCALE = 2;

    public TranRecord {
        tranId = CobolText.fit(tranId, 16);
        tranTypeCd = CobolText.fit(tranTypeCd, 2);
        tranSource = CobolText.fit(tranSource, 10);
        tranDesc = CobolText.fit(tranDesc, 100);
        tranAmt = tranAmt.setScale(TRAN_AMT_SCALE, java.math.RoundingMode.DOWN);
        tranMerchantName = CobolText.fit(tranMerchantName, 50);
        tranMerchantCity = CobolText.fit(tranMerchantCity, 50);
        tranMerchantZip = CobolText.fit(tranMerchantZip, 10);
        tranCardNum = CobolText.fit(tranCardNum, 16);
        tranOrigTs = CobolText.fit(tranOrigTs, 26);
        tranProcTs = CobolText.fit(tranProcTs, 26);
    }

    /** Parse a fixed-width 350-byte record (shorter lines are space padded). */
    public static TranRecord parse(String line) {
        String r = CobolText.fit(line, RECORD_LENGTH);
        return new TranRecord(
                r.substring(0, 16),
                r.substring(16, 18),
                Integer.parseInt(r.substring(18, 22)),
                r.substring(22, 32),
                r.substring(32, 132),
                ZonedDecimal.parse(r.substring(132, 143), TRAN_AMT_SCALE),
                Long.parseLong(r.substring(143, 152)),
                r.substring(152, 202),
                r.substring(202, 252),
                r.substring(252, 262),
                r.substring(262, 278),
                r.substring(278, 304),
                r.substring(304, 330));
    }

    public String toFixedWidth() {
        return tranId
                + tranTypeCd
                + CobolText.unsignedDigits(tranCatCd, 4)
                + tranSource
                + tranDesc
                + ZonedDecimal.format(tranAmt, TRAN_AMT_DIGITS, TRAN_AMT_SCALE)
                + CobolText.unsignedDigits(tranMerchantId, 9)
                + tranMerchantName
                + tranMerchantCity
                + tranMerchantZip
                + tranCardNum
                + tranOrigTs
                + tranProcTs
                + CobolText.spaces(20);
    }
}
