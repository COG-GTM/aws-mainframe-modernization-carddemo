package com.carddemo.card;

import com.carddemo.common.data.CobolField;
import com.carddemo.common.data.CopybookRecordMapper;

/**
 * One CARDXREF record (copybook CVACT03Y) as an immutable value. {@link #MAPPER} converts it to and from
 * the fixed-width record; {@link CardXref} persists it.
 *
 * @param cardNum XREF-CARD-NUM PIC X(16)
 * @param custId XREF-CUST-ID PIC 9(09)
 * @param acctId XREF-ACCT-ID PIC 9(11)
 */
public record CardXrefRecord(
        @CobolField("XREF-CARD-NUM") String cardNum,
        @CobolField("XREF-CUST-ID") int custId,
        @CobolField("XREF-ACCT-ID") long acctId) {

    public static final CopybookRecordMapper<CardXrefRecord> MAPPER =
            CopybookRecordMapper.of(CardXrefRecord.class, "CVACT03Y");
}
