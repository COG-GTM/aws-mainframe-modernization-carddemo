package com.carddemo.transaction.domain;

import java.math.BigDecimal;

/**
 * DALYTRAN-RECORD (copybook CVTRA06Y, RECLN 350) - one record of the daily transaction sequential file.
 * PIC X fields keep their fixed-length space padding; {@code rawRecord} is the exact 350 byte image
 * (needed for REJECT-TRAN-DATA in DALYREJS).
 */
public record DailyTransaction(
        String id,            // DALYTRAN-ID            PIC X(16)
        String typeCd,        // DALYTRAN-TYPE-CD       PIC X(02)
        int catCd,            // DALYTRAN-CAT-CD        PIC 9(04)
        String source,        // DALYTRAN-SOURCE        PIC X(10)
        String desc,          // DALYTRAN-DESC          PIC X(100)
        BigDecimal amt,       // DALYTRAN-AMT           PIC S9(09)V99
        long merchantId,      // DALYTRAN-MERCHANT-ID   PIC 9(09)
        String merchantName,  // DALYTRAN-MERCHANT-NAME PIC X(50)
        String merchantCity,  // DALYTRAN-MERCHANT-CITY PIC X(50)
        String merchantZip,   // DALYTRAN-MERCHANT-ZIP  PIC X(10)
        String cardNum,       // DALYTRAN-CARD-NUM      PIC X(16)
        String origTs,        // DALYTRAN-ORIG-TS       PIC X(26)
        String procTs,        // DALYTRAN-PROC-TS       PIC X(26)
        String rawRecord) {

    public static final int RECORD_LENGTH = 350;
}
