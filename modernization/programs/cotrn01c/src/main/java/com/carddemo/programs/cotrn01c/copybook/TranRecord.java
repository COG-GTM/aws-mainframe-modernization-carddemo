package com.carddemo.programs.cotrn01c.copybook;

import java.math.BigDecimal;

/**
 * CVTRA05Y - 01 TRAN-RECORD (RECLN 350), the TRANSACT VSAM KSDS record keyed by TRAN-ID.
 *
 * @param tranId           TRAN-ID PIC X(16)
 * @param tranTypeCd       TRAN-TYPE-CD PIC X(02)
 * @param tranCatCd        TRAN-CAT-CD PIC 9(04)
 * @param tranSource       TRAN-SOURCE PIC X(10)
 * @param tranDesc         TRAN-DESC PIC X(100)
 * @param tranAmt          TRAN-AMT PIC S9(09)V99 (scale 2)
 * @param tranMerchantId   TRAN-MERCHANT-ID PIC 9(09)
 * @param tranMerchantName TRAN-MERCHANT-NAME PIC X(50)
 * @param tranMerchantCity TRAN-MERCHANT-CITY PIC X(50)
 * @param tranMerchantZip  TRAN-MERCHANT-ZIP PIC X(10)
 * @param tranCardNum      TRAN-CARD-NUM PIC X(16)
 * @param tranOrigTs       TRAN-ORIG-TS PIC X(26)
 * @param tranProcTs       TRAN-PROC-TS PIC X(26)
 */
public record TranRecord(
        String tranId,
        String tranTypeCd,
        int tranCatCd,
        String tranSource,
        String tranDesc,
        BigDecimal tranAmt,
        long tranMerchantId,
        String tranMerchantName,
        String tranMerchantCity,
        String tranMerchantZip,
        String tranCardNum,
        String tranOrigTs,
        String tranProcTs) {

    public static final int RECORD_LENGTH = 350;
    public static final int TRAN_ID_LENGTH = 16;
    public static final int TRAN_AMT_INT_DIGITS = 9;
    public static final int TRAN_AMT_SCALE = 2;

    /** Normalises every field to its PIC width (space padding / truncation, scale 2). */
    public TranRecord {
        tranId = Cobol.moveAlnum(tranId, 16);
        tranTypeCd = Cobol.moveAlnum(tranTypeCd, 2);
        tranSource = Cobol.moveAlnum(tranSource, 10);
        tranDesc = Cobol.moveAlnum(tranDesc, 100);
        tranAmt = tranAmt.setScale(TRAN_AMT_SCALE, java.math.RoundingMode.DOWN);
        tranMerchantName = Cobol.moveAlnum(tranMerchantName, 50);
        tranMerchantCity = Cobol.moveAlnum(tranMerchantCity, 50);
        tranMerchantZip = Cobol.moveAlnum(tranMerchantZip, 10);
        tranCardNum = Cobol.moveAlnum(tranCardNum, 16);
        tranOrigTs = Cobol.moveAlnum(tranOrigTs, 26);
        tranProcTs = Cobol.moveAlnum(tranProcTs, 26);
    }

    /** Parses one fixed-width 350-byte record (display format, trailing-overpunch TRAN-AMT). */
    public static TranRecord parse(String line) {
        String r = Cobol.moveAlnum(line, RECORD_LENGTH);
        return new TranRecord(
                r.substring(0, 16),
                r.substring(16, 18),
                (int) Cobol.parseUnsigned(r.substring(18, 22)),
                r.substring(22, 32),
                r.substring(32, 132),
                Cobol.parseZonedSigned(r.substring(132, 143), TRAN_AMT_SCALE),
                Cobol.parseUnsigned(r.substring(143, 152)),
                r.substring(152, 202),
                r.substring(202, 252),
                r.substring(252, 262),
                r.substring(262, 278),
                r.substring(278, 304),
                r.substring(304, 330));
    }

    /** Serialises to the fixed-width 350-byte layout (FILLER X(20) as spaces). */
    public String format() {
        return tranId + tranTypeCd + Cobol.moveUnsignedNumeric(tranCatCd, 4) + tranSource + tranDesc
                + Cobol.formatZonedSigned(tranAmt, TRAN_AMT_INT_DIGITS, TRAN_AMT_SCALE)
                + Cobol.moveUnsignedNumeric(tranMerchantId, 9) + tranMerchantName + tranMerchantCity
                + tranMerchantZip + tranCardNum + tranOrigTs + tranProcTs + Cobol.spaces(20);
    }
}
