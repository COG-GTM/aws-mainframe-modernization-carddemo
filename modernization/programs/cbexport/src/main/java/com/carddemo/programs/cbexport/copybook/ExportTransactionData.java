package com.carddemo.programs.cbexport.copybook;

import java.math.BigDecimal;

/**
 * CVEXPORT - 05 EXPORT-TRANSACTION-DATA REDEFINES EXPORT-RECORD-DATA (460 bytes).
 *
 * @param tranId          EXP-TRAN-ID PIC X(16)
 * @param typeCd          EXP-TRAN-TYPE-CD PIC X(02)
 * @param catCd           EXP-TRAN-CAT-CD PIC 9(04)
 * @param source          EXP-TRAN-SOURCE PIC X(10)
 * @param desc            EXP-TRAN-DESC PIC X(100)
 * @param amt             EXP-TRAN-AMT PIC S9(09)V99 COMP-3 (6 bytes)
 * @param merchantId      EXP-TRAN-MERCHANT-ID PIC 9(09) COMP (4 bytes)
 * @param merchantName    EXP-TRAN-MERCHANT-NAME PIC X(50)
 * @param merchantCity    EXP-TRAN-MERCHANT-CITY PIC X(50)
 * @param merchantZip     EXP-TRAN-MERCHANT-ZIP PIC X(10)
 * @param cardNum         EXP-TRAN-CARD-NUM PIC X(16)
 * @param origTs          EXP-TRAN-ORIG-TS PIC X(26)
 * @param procTs          EXP-TRAN-PROC-TS PIC X(26)
 */
public record ExportTransactionData(
        String tranId,
        String typeCd,
        int catCd,
        String source,
        String desc,
        BigDecimal amt,
        long merchantId,
        String merchantName,
        String merchantCity,
        String merchantZip,
        String cardNum,
        String origTs,
        String procTs) {
}
