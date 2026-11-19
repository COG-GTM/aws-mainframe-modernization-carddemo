package com.carddemo.card.domain;

import com.carddemo.common.record.FixedRecord;

import static com.carddemo.common.record.Copybooks.CVACT03Y;

/** {@link CardXref} ↔ CVACT03Y record. */
public final class CardXrefRecords {

    private CardXrefRecords() {
    }

    public static CardXref fromRecord(String text) {
        FixedRecord r = CVACT03Y.parse(text);
        CardXref x = new CardXref();
        x.setCardNum(r.text("XREF-CARD-NUM"));
        x.setCustId(r.number("XREF-CUST-ID"));
        x.setAcctId(r.number("XREF-ACCT-ID"));
        return x;
    }

    public static String toRecord(CardXref x) {
        return CVACT03Y.blank()
                .set("XREF-CARD-NUM", x.getCardNum())
                .set("XREF-CUST-ID", x.getCustId())
                .set("XREF-ACCT-ID", x.getAcctId())
                .toString();
    }
}
