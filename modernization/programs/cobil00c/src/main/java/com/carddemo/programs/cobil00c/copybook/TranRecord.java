package com.carddemo.programs.cobil00c.copybook;

import java.math.BigDecimal;

/** CVTRA05Y {@code TRAN-RECORD} (RECLN 350). */
public record TranRecord(
        String tranId,            // PIC X(16)
        String tranTypeCd,        // PIC X(02)
        int tranCatCd,            // PIC 9(04)
        String tranSource,        // PIC X(10)
        String tranDesc,          // PIC X(100)
        BigDecimal tranAmt,       // PIC S9(09)V99
        long tranMerchantId,      // PIC 9(09)
        String tranMerchantName,  // PIC X(50)
        String tranMerchantCity,  // PIC X(50)
        String tranMerchantZip,   // PIC X(10)
        String tranCardNum,       // PIC X(16)
        String tranOrigTs,        // PIC X(26)
        String tranProcTs) {      // PIC X(26); FILLER X(20) dropped

    public static final int RECORD_LENGTH = 350;
    public static final int TRAN_ID_LENGTH = 16;
    public static final int AMT_INTEGER_DIGITS = 9;
    public static final int AMT_SCALE = 2;

    public TranRecord {
        tranId = Cobol.alnum(tranId, 16);
        tranTypeCd = Cobol.alnum(tranTypeCd, 2);
        tranCatCd = (int) (Math.abs((long) tranCatCd) % 10_000);
        tranSource = Cobol.alnum(tranSource, 10);
        tranDesc = Cobol.alnum(tranDesc, 100);
        tranAmt = Cobol.toPic(tranAmt == null ? BigDecimal.ZERO : tranAmt, AMT_INTEGER_DIGITS, AMT_SCALE);
        tranMerchantId = Math.abs(tranMerchantId) % 1_000_000_000L;
        tranMerchantName = Cobol.alnum(tranMerchantName, 50);
        tranMerchantCity = Cobol.alnum(tranMerchantCity, 50);
        tranMerchantZip = Cobol.alnum(tranMerchantZip, 10);
        tranCardNum = Cobol.alnum(tranCardNum, 16);
        tranOrigTs = Cobol.alnum(tranOrigTs, 26);
        tranProcTs = Cobol.alnum(tranProcTs, 26);
    }

    /** {@code INITIALIZE TRAN-RECORD}: alphanumerics to SPACES, numerics to ZERO. */
    public static TranRecord initialized() {
        return new TranRecord("", "", 0, "", "", BigDecimal.ZERO, 0, "", "", "", "", "", "");
    }
}
