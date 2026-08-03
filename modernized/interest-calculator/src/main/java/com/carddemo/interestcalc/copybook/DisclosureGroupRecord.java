package com.carddemo.interestcalc.copybook;

import java.math.BigDecimal;

/**
 * Copybook {@code CVTRA02Y} - {@code DIS-GROUP-RECORD}, RECLN 50.
 *
 * <pre>
 * 05 DIS-GROUP-KEY.
 *    10 DIS-ACCT-GROUP-ID  PIC X(10).     bytes  1-10
 *    10 DIS-TRAN-TYPE-CD   PIC X(02).     bytes 11-12
 *    10 DIS-TRAN-CAT-CD    PIC 9(04).     bytes 13-16
 * 05 DIS-INT-RATE          PIC S9(04)V99. bytes 17-22
 * 05 FILLER                PIC X(28).     bytes 23-50
 * </pre>
 *
 * @param interestRate {@code DIS-INT-RATE} - an annual percentage, scale 2 (15.00 means 15%)
 */
public record DisclosureGroupRecord(String accountGroupId, String tranTypeCode, String tranCatCode,
                                    BigDecimal interestRate) {

    public static final int LENGTH = 50;

    public static DisclosureGroupRecord parse(String raw) {
        String rec = FixedWidth.record(raw, LENGTH);
        return new DisclosureGroupRecord(
                FixedWidth.field(rec, 0, 10),
                FixedWidth.field(rec, 10, 2),
                FixedWidth.field(rec, 12, 4),
                ZonedDecimal.decode(FixedWidth.field(rec, 16, 6), 4, 2));
    }

    /** The VSAM {@code RECORD KEY} of DISCGRP: group id (X(10)) + type code + category code. */
    public static String key(String accountGroupId, String tranTypeCode, String tranCatCode) {
        return FixedWidth.alphanumeric(accountGroupId.stripTrailing(), 10) + tranTypeCode + tranCatCode;
    }

    public String key() {
        return accountGroupId + tranTypeCode + tranCatCode;
    }
}
