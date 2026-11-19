package com.carddemo.card.batch;

import com.carddemo.card.domain.CardXref;
import com.carddemo.common.cobol.CodePage;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.RecordLayout;

/** CVACT03Y CARD-XREF-RECORD (CARDXREF, 50 bytes) and its mapping to {@link CardXref}. */
public final class CardXrefRecordLayout {

    public static final RecordLayout LAYOUT = RecordLayout.builder("CVACT03Y", 50)
            .alphanumeric("XREF-CARD-NUM", 16)
            .zoned("XREF-CUST-ID", 9, 0, false)
            .zoned("XREF-ACCT-ID", 11, 0, false)
            .filler(14)
            .build();

    private CardXrefRecordLayout() {
    }

    public static CardXref toEntity(CobolRecord r) {
        return new CardXref(r.getString("XREF-CARD-NUM"), r.getLong("XREF-CUST-ID"), r.getLong("XREF-ACCT-ID"));
    }

    public static CobolRecord toRecord(CardXref x, CodePage codePage) {
        return LAYOUT.blank(codePage)
                .setString("XREF-CARD-NUM", x.getCardNum())
                .setLong("XREF-CUST-ID", x.getCustId())
                .setLong("XREF-ACCT-ID", x.getAcctId());
    }
}
