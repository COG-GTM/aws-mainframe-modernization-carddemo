package com.carddemo.interest.model;

import com.carddemo.interest.CobolDecimal;

/**
 * Copybook CVACT03Y — card cross reference (RECLN 50).
 *
 * <pre>
 * 05 XREF-CARD-NUM PIC X(16)
 * 05 XREF-CUST-ID  PIC 9(09)
 * 05 XREF-ACCT-ID  PIC 9(11)
 * </pre>
 */
public record CardXrefRecord(String cardNumber, String customerId, String acctId) {

    public CardXrefRecord {
        cardNumber = CobolDecimal.alphanumeric(cardNumber, 16);
        customerId = CobolDecimal.zoned(customerId, 9);
        acctId = CobolDecimal.zoned(acctId, 11);
    }
}
