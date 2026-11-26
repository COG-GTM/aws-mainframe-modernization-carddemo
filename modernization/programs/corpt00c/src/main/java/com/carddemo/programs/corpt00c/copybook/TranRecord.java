package com.carddemo.programs.corpt00c.copybook;

import com.carddemo.programs.corpt00c.CobolText;

import java.math.BigDecimal;

/** CVTRA05Y - 01 TRAN-RECORD (RECLN 350), the TRANSACT file layout read by the TRANREPT job. */
public record TranRecord(
        String tranId,            // X(16)
        String tranTypeCd,        // X(02)
        int tranCatCd,            // 9(04)
        String tranSource,        // X(10)
        String tranDesc,          // X(100)
        BigDecimal tranAmt,       // S9(09)V99
        long tranMerchantId,      // 9(09)
        String tranMerchantName,  // X(50)
        String tranMerchantCity,  // X(50)
        String tranMerchantZip,   // X(10)
        String tranCardNum,       // X(16)
        String tranOrigTs,        // X(26)
        String tranProcTs) {      // X(26)  (+ FILLER X(20))

    public static final int LENGTH = 350;

    /** Zero-based byte offsets of each field. */
    public static final int OFF_TRAN_ID = 0;
    public static final int OFF_TRAN_TYPE_CD = 16;
    public static final int OFF_TRAN_CAT_CD = 18;
    public static final int OFF_TRAN_SOURCE = 22;
    public static final int OFF_TRAN_DESC = 32;
    public static final int OFF_TRAN_AMT = 132;
    public static final int OFF_TRAN_MERCHANT_ID = 143;
    public static final int OFF_TRAN_MERCHANT_NAME = 152;
    public static final int OFF_TRAN_MERCHANT_CITY = 202;
    public static final int OFF_TRAN_MERCHANT_ZIP = 252;
    public static final int OFF_TRAN_CARD_NUM = 262;
    public static final int OFF_TRAN_ORIG_TS = 278;
    public static final int OFF_TRAN_PROC_TS = 304;
    public static final int OFF_FILLER = 330;

    public static TranRecord fromFixedWidth(String line) {
        String s = CobolText.move(line, LENGTH);
        return new TranRecord(
                s.substring(OFF_TRAN_ID, OFF_TRAN_TYPE_CD),
                s.substring(OFF_TRAN_TYPE_CD, OFF_TRAN_CAT_CD),
                Integer.parseInt(s.substring(OFF_TRAN_CAT_CD, OFF_TRAN_SOURCE)),
                s.substring(OFF_TRAN_SOURCE, OFF_TRAN_DESC),
                s.substring(OFF_TRAN_DESC, OFF_TRAN_AMT),
                CobolText.decodeZonedSigned(s.substring(OFF_TRAN_AMT, OFF_TRAN_MERCHANT_ID), 2),
                Long.parseLong(s.substring(OFF_TRAN_MERCHANT_ID, OFF_TRAN_MERCHANT_NAME)),
                s.substring(OFF_TRAN_MERCHANT_NAME, OFF_TRAN_MERCHANT_CITY),
                s.substring(OFF_TRAN_MERCHANT_CITY, OFF_TRAN_MERCHANT_ZIP),
                s.substring(OFF_TRAN_MERCHANT_ZIP, OFF_TRAN_CARD_NUM),
                s.substring(OFF_TRAN_CARD_NUM, OFF_TRAN_ORIG_TS),
                s.substring(OFF_TRAN_ORIG_TS, OFF_TRAN_PROC_TS),
                s.substring(OFF_TRAN_PROC_TS, OFF_FILLER));
    }

    public String toFixedWidth() {
        return CobolText.move(tranId, 16)
                + CobolText.move(tranTypeCd, 2)
                + String.format("%04d", tranCatCd)
                + CobolText.move(tranSource, 10)
                + CobolText.move(tranDesc, 100)
                + CobolText.encodeZonedSigned(tranAmt, 9, 2)
                + String.format("%09d", tranMerchantId)
                + CobolText.move(tranMerchantName, 50)
                + CobolText.move(tranMerchantCity, 50)
                + CobolText.move(tranMerchantZip, 10)
                + CobolText.move(tranCardNum, 16)
                + CobolText.move(tranOrigTs, 26)
                + CobolText.move(tranProcTs, 26)
                + CobolText.spaces(20);
    }
}
