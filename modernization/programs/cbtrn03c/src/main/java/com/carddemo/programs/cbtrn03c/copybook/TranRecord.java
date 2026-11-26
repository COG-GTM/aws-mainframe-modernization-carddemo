package com.carddemo.programs.cbtrn03c.copybook;

import com.carddemo.programs.cbtrn03c.support.Fixed;
import com.carddemo.programs.cbtrn03c.support.ZonedDecimal;

import java.math.BigDecimal;

/**
 * CVTRA05Y - 01 TRAN-RECORD (RECLN 350).
 *
 * <pre>
 * 05 TRAN-ID            PIC X(16)      offset   0
 * 05 TRAN-TYPE-CD       PIC X(02)      offset  16
 * 05 TRAN-CAT-CD        PIC 9(04)      offset  18
 * 05 TRAN-SOURCE        PIC X(10)      offset  22
 * 05 TRAN-DESC          PIC X(100)     offset  32
 * 05 TRAN-AMT           PIC S9(09)V99  offset 132 (zoned, trailing overpunch)
 * 05 TRAN-MERCHANT-ID   PIC 9(09)      offset 143
 * 05 TRAN-MERCHANT-NAME PIC X(50)      offset 152
 * 05 TRAN-MERCHANT-CITY PIC X(50)      offset 202
 * 05 TRAN-MERCHANT-ZIP  PIC X(10)      offset 252
 * 05 TRAN-CARD-NUM      PIC X(16)      offset 262
 * 05 TRAN-ORIG-TS       PIC X(26)      offset 278
 * 05 TRAN-PROC-TS       PIC X(26)      offset 304
 * 05 FILLER             PIC X(20)      offset 330
 * </pre>
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
        String tranProcTs,
        String filler) {

    public static final int LENGTH = 350;
    public static final int TRAN_AMT_OFFSET = 132;
    public static final int TRAN_AMT_LENGTH = 11;
    public static final int TRAN_AMT_INTEGER_DIGITS = 9;
    public static final int TRAN_AMT_SCALE = 2;

    public TranRecord {
        tranId = Fixed.pic(tranId, 16);
        tranTypeCd = Fixed.pic(tranTypeCd, 2);
        tranSource = Fixed.pic(tranSource, 10);
        tranDesc = Fixed.pic(tranDesc, 100);
        tranMerchantName = Fixed.pic(tranMerchantName, 50);
        tranMerchantCity = Fixed.pic(tranMerchantCity, 50);
        tranMerchantZip = Fixed.pic(tranMerchantZip, 10);
        tranCardNum = Fixed.pic(tranCardNum, 16);
        tranOrigTs = Fixed.pic(tranOrigTs, 26);
        tranProcTs = Fixed.pic(tranProcTs, 26);
        filler = Fixed.pic(filler, 20);
        tranAmt = tranAmt == null ? BigDecimal.ZERO.setScale(TRAN_AMT_SCALE) : tranAmt.setScale(TRAN_AMT_SCALE);
    }

    /** Working-storage initial state (alphanumerics spaces, numerics zero). */
    public static TranRecord initial() {
        return new TranRecord("", "", 0, "", "", BigDecimal.ZERO, 0L, "", "", "", "", "", "", "");
    }

    public static TranRecord parse(String record) {
        String r = Fixed.pic(record, LENGTH);
        return new TranRecord(
                r.substring(0, 16),
                r.substring(16, 18),
                (int) Fixed.parseUnsigned(r.substring(18, 22), "TRAN-CAT-CD"),
                r.substring(22, 32),
                r.substring(32, 132),
                ZonedDecimal.decode(r.substring(132, 143), TRAN_AMT_SCALE),
                Fixed.parseUnsigned(r.substring(143, 152), "TRAN-MERCHANT-ID"),
                r.substring(152, 202),
                r.substring(202, 252),
                r.substring(252, 262),
                r.substring(262, 278),
                r.substring(278, 304),
                r.substring(304, 330),
                r.substring(330, 350));
    }

    public String toFixedWidth() {
        return tranId
                + tranTypeCd
                + Fixed.unsigned(tranCatCd, 4)
                + tranSource
                + tranDesc
                + ZonedDecimal.encode(tranAmt, TRAN_AMT_INTEGER_DIGITS, TRAN_AMT_SCALE)
                + Fixed.unsigned(tranMerchantId, 9)
                + tranMerchantName
                + tranMerchantCity
                + tranMerchantZip
                + tranCardNum
                + tranOrigTs
                + tranProcTs
                + filler;
    }
}
