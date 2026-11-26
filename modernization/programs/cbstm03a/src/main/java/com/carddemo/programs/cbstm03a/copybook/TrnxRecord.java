package com.carddemo.programs.cbstm03a.copybook;

import com.carddemo.programs.cbstm03a.support.Pic;
import java.math.BigDecimal;

/**
 * COSTM01 - TRNX-RECORD, the transaction layout re-keyed by card number for reporting
 * (350 bytes; TRNX-KEY = TRNX-CARD-NUM + TRNX-ID, TRNX-REST = the remaining 318 bytes).
 */
public record TrnxRecord(
        String trnxCardNum,      // TRNX-CARD-NUM        PIC X(16)
        String trnxId,           // TRNX-ID              PIC X(16)
        String trnxTypeCd,       // TRNX-TYPE-CD         PIC X(02)
        int trnxCatCd,           // TRNX-CAT-CD          PIC 9(04)
        String trnxSource,       // TRNX-SOURCE          PIC X(10)
        String trnxDesc,         // TRNX-DESC            PIC X(100)
        BigDecimal trnxAmt,      // TRNX-AMT             PIC S9(09)V99
        long trnxMerchantId,     // TRNX-MERCHANT-ID     PIC 9(09)
        String trnxMerchantName, // TRNX-MERCHANT-NAME   PIC X(50)
        String trnxMerchantCity, // TRNX-MERCHANT-CITY   PIC X(50)
        String trnxMerchantZip,  // TRNX-MERCHANT-ZIP    PIC X(10)
        String trnxOrigTs,       // TRNX-ORIG-TS         PIC X(26)
        String trnxProcTs,       // TRNX-PROC-TS         PIC X(26)
        String filler) {         // FILLER               PIC X(20)

    public static final int LENGTH = 350;
    public static final int KEY_LENGTH = 32;
    public static final int CARD_NUM_LENGTH = 16;
    public static final int ID_LENGTH = 16;
    public static final int REST_LENGTH = 318;

    public static TrnxRecord parse(String raw) {
        String r = Pic.x(raw, LENGTH);
        return new TrnxRecord(
                r.substring(0, 16),
                r.substring(16, 32),
                r.substring(32, 34),
                (int) Pic.unsigned(r.substring(34, 38)),
                r.substring(38, 48),
                r.substring(48, 148),
                Pic.zoned(r.substring(148, 159), 9, 2),
                Pic.unsigned(r.substring(159, 168)),
                r.substring(168, 218),
                r.substring(218, 268),
                r.substring(268, 278),
                r.substring(278, 304),
                r.substring(304, 330),
                r.substring(330, 350));
    }

    /** Group MOVE of TRNX-CARD-NUM, TRNX-ID and the 318-byte TRNX-REST. */
    public static TrnxRecord of(String cardNum, String id, String rest) {
        return parse(Pic.x(cardNum, 16) + Pic.x(id, 16) + Pic.x(rest, REST_LENGTH));
    }

    public String toFixedWidth() {
        return Pic.x(trnxCardNum, 16) + Pic.x(trnxId, 16) + rest();
    }

    /** TRNX-REST group (PIC X(318) as an alphanumeric item). */
    public String rest() {
        return Pic.x(trnxTypeCd, 2)
                + Pic.unsigned(trnxCatCd, 4)
                + Pic.x(trnxSource, 10)
                + Pic.x(trnxDesc, 100)
                + Pic.zoned(trnxAmt, 9, 2)
                + Pic.unsigned(trnxMerchantId, 9)
                + Pic.x(trnxMerchantName, 50)
                + Pic.x(trnxMerchantCity, 50)
                + Pic.x(trnxMerchantZip, 10)
                + Pic.x(trnxOrigTs, 26)
                + Pic.x(trnxProcTs, 26)
                + Pic.x(filler, 20);
    }
}
