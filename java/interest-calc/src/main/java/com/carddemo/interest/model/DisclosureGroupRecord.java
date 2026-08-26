package com.carddemo.interest.model;

import java.math.BigDecimal;

/**
 * Copybook CVTRA02Y — disclosure group (RECLN 50).
 *
 * <pre>
 * 05 DIS-GROUP-KEY.
 *    10 DIS-ACCT-GROUP-ID PIC X(10)
 *    10 DIS-TRAN-TYPE-CD  PIC X(02)
 *    10 DIS-TRAN-CAT-CD   PIC 9(04)
 * 05 DIS-INT-RATE         PIC S9(04)V99
 * </pre>
 */
public record DisclosureGroupRecord(String acctGroupId, String tranTypeCd, String tranCatCd, BigDecimal interestRate) {

    /** Group id substituted by 1200-A-GET-DEFAULT-INT-RATE when the account's group has no record. */
    public static final String DEFAULT_GROUP_ID = "DEFAULT";

    public DisclosureGroupRecord {
        acctGroupId = com.carddemo.interest.CobolDecimal.alphanumeric(acctGroupId, 10);
        tranTypeCd = com.carddemo.interest.CobolDecimal.alphanumeric(tranTypeCd, 2);
        tranCatCd = com.carddemo.interest.CobolDecimal.zoned(tranCatCd, 4);
        interestRate = com.carddemo.interest.CobolDecimal.toPicture(interestRate, 4);
    }

    public DisclosureGroupKey key() {
        return new DisclosureGroupKey(acctGroupId, tranTypeCd, tranCatCd);
    }
}
