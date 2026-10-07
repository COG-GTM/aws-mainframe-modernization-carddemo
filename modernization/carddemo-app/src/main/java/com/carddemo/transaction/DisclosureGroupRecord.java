package com.carddemo.transaction;

import com.carddemo.common.data.CobolField;
import com.carddemo.common.data.CopybookRecordMapper;
import java.math.BigDecimal;

/**
 * One DISCGRP record (copybook CVTRA02Y) as an immutable value. {@link #MAPPER} converts it to and from
 * the fixed-width record; {@link DisclosureGroup} persists it.
 *
 * @param acctGroupId DIS-ACCT-GROUP-ID PIC X(10)
 * @param tranTypeCd DIS-TRAN-TYPE-CD PIC X(02)
 * @param tranCatCd DIS-TRAN-CAT-CD PIC 9(04)
 * @param intRate DIS-INT-RATE PIC S9(04)V99
 */
public record DisclosureGroupRecord(
        @CobolField("DIS-ACCT-GROUP-ID") String acctGroupId,
        @CobolField("DIS-TRAN-TYPE-CD") String tranTypeCd,
        @CobolField("DIS-TRAN-CAT-CD") int tranCatCd,
        @CobolField("DIS-INT-RATE") BigDecimal intRate) {

    public static final CopybookRecordMapper<DisclosureGroupRecord> MAPPER =
            CopybookRecordMapper.of(DisclosureGroupRecord.class, "CVTRA02Y");
}
