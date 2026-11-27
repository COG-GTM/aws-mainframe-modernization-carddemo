package com.carddemo.data.record;

import com.carddemo.common.codec.CobolField;

import java.math.BigDecimal;

/** Fixed-width DIS-GROUP-RECORD. Layout: copybook CVTRA02Y. */
public record DisclosureGroupRecord(
        @CobolField("DIS-GROUP-KEY") Key id,
        @CobolField("DIS-INT-RATE") BigDecimal interestRate) {

    /** DIS-GROUP-KEY. */
    public record Key(
            @CobolField("DIS-ACCT-GROUP-ID") String groupId,
            @CobolField("DIS-TRAN-TYPE-CD") String typeCode,
            @CobolField("DIS-TRAN-CAT-CD") Integer categoryCode) {
    }
}
