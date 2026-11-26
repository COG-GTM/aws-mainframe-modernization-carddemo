package com.carddemo.programs.cbimport.copybook;

import java.math.BigDecimal;

/** CVTRA05Y: TRAN-RECORD, RECLN 350 (TRNXOUT, LRECL=350). FILLER X(20) written as spaces. */
public record TranRecord(
        String tranId,                  // TRAN-ID X(16)
        String typeCd,                  // TRAN-TYPE-CD X(02)
        int catCd,                      // TRAN-CAT-CD 9(04)
        String source,                  // TRAN-SOURCE X(10)
        String desc,                    // TRAN-DESC X(100)
        BigDecimal amt,                 // TRAN-AMT S9(09)V99
        long merchantId,                // TRAN-MERCHANT-ID 9(09)
        String merchantName,            // TRAN-MERCHANT-NAME X(50)
        String merchantCity,            // TRAN-MERCHANT-CITY X(50)
        String merchantZip,             // TRAN-MERCHANT-ZIP X(10)
        String cardNum,                 // TRAN-CARD-NUM X(16)
        String origTs,                  // TRAN-ORIG-TS X(26)
        String procTs) {                // TRAN-PROC-TS X(26)

    public static final int LENGTH = 350;
    public static final int FILLER_LENGTH = 20;

    public TranRecord {
        tranId = Pic.alnum(tranId, 16);
        typeCd = Pic.alnum(typeCd, 2);
        catCd = (int) Pic.unsigned(catCd, 4);
        source = Pic.alnum(source, 10);
        desc = Pic.alnum(desc, 100);
        amt = Pic.signed(amt, 9, 2);
        merchantId = Pic.unsigned(merchantId, 9);
        merchantName = Pic.alnum(merchantName, 50);
        merchantCity = Pic.alnum(merchantCity, 50);
        merchantZip = Pic.alnum(merchantZip, 10);
        cardNum = Pic.alnum(cardNum, 16);
        origTs = Pic.alnum(origTs, 26);
        procTs = Pic.alnum(procTs, 26);
    }

    /** INITIALIZE TRAN-RECORD. */
    public static TranRecord initialized() {
        return new TranRecord("", "", 0, "", "", BigDecimal.ZERO, 0, "", "", "", "", "", "");
    }

    public String toFixedWidth() {
        return tranId + typeCd + Pic.displayUnsigned(catCd, 4) + source + desc
                + Pic.displaySigned(amt, 9, 2) + Pic.displayUnsigned(merchantId, 9)
                + merchantName + merchantCity + merchantZip + cardNum + origTs + procTs
                + " ".repeat(FILLER_LENGTH);
    }

    public static TranRecord parse(String line) {
        Slicer s = new Slicer(line, LENGTH);
        return new TranRecord(s.x(16), s.x(2), (int) s.unsigned(4), s.x(10), s.x(100), s.signed(9, 2),
                s.unsigned(9), s.x(50), s.x(50), s.x(10), s.x(16), s.x(26), s.x(26));
    }
}
