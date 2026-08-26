package com.carddemo.interest.model;

import java.math.BigDecimal;

/**
 * Copybook CVTRA01Y — transaction category balance (RECLN 50).
 *
 * <pre>
 * 05 TRAN-CAT-KEY.
 *    10 TRANCAT-ACCT-ID   PIC 9(11)
 *    10 TRANCAT-TYPE-CD   PIC X(02)
 *    10 TRANCAT-CD        PIC 9(04)
 * 05 TRAN-CAT-BAL         PIC S9(09)V99
 * </pre>
 */
public record TranCatBalRecord(String acctId, String typeCd, String categoryCd, BigDecimal balance) {

    public TranCatBalRecord {
        acctId = com.carddemo.interest.CobolDecimal.zoned(acctId, 11);
        typeCd = com.carddemo.interest.CobolDecimal.alphanumeric(typeCd, 2);
        categoryCd = com.carddemo.interest.CobolDecimal.zoned(categoryCd, 4);
        balance = com.carddemo.interest.CobolDecimal.toAmount(balance);
    }
}
