package com.carddemo.interest.io.codec;

import com.carddemo.interest.domain.AccountId;
import com.carddemo.interest.domain.CardXref;
import com.carddemo.mainframe.io.RecordLayout;
import com.carddemo.mainframe.io.layout.CardDemoLayouts;

/** Maps {@code CVACT03Y} records to {@link CardXref} instances. */
public final class CardXrefCodec {

    private static final RecordLayout LAYOUT = CardDemoLayouts.CARD_XREF;

    private CardXrefCodec() {
    }

    public static int recordLength() {
        return LAYOUT.recordLength();
    }

    public static CardXref decode(byte[] record) {
        return new CardXref(
                LAYOUT.rawText(record, "cardNumber"),
                LAYOUT.rawText(record, "customerId"),
                AccountId.of(LAYOUT.rawText(record, "accountId")));
    }
}
