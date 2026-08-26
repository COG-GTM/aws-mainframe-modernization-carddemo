package com.carddemo.interest.model;

/** Record key of the DISCGRP KSDS: DIS-ACCT-GROUP-ID + DIS-TRAN-TYPE-CD + DIS-TRAN-CAT-CD. */
public record DisclosureGroupKey(String acctGroupId, String tranTypeCd, String tranCatCd) {

    public DisclosureGroupKey {
        acctGroupId = com.carddemo.interest.CobolDecimal.alphanumeric(acctGroupId, 10);
        tranTypeCd = com.carddemo.interest.CobolDecimal.alphanumeric(tranTypeCd, 2);
        tranCatCd = com.carddemo.interest.CobolDecimal.zoned(tranCatCd, 4);
    }

    public DisclosureGroupKey withDefaultGroup() {
        return new DisclosureGroupKey(DisclosureGroupRecord.DEFAULT_GROUP_ID, tranTypeCd, tranCatCd);
    }
}
