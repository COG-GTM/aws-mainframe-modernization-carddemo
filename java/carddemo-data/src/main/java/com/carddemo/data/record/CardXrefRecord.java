package com.carddemo.data.record;

import com.carddemo.common.codec.CobolField;

/** Fixed-width CARD-XREF-RECORD. Layout: copybook CVACT03Y. */
public record CardXrefRecord(
        @CobolField("XREF-CARD-NUM") String cardNumber,
        @CobolField("XREF-CUST-ID") Long customerId,
        @CobolField("XREF-ACCT-ID") Long accountId) {
}
