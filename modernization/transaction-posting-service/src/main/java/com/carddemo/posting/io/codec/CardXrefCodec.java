package com.carddemo.posting.io.codec;

import com.carddemo.mainframe.io.RecordLayout;
import com.carddemo.mainframe.io.layout.CardDemoLayouts;
import com.carddemo.posting.domain.AccountId;
import com.carddemo.posting.domain.CardNumber;
import com.carddemo.posting.domain.CardXref;

/** Maps {@code CVACT03Y} cross-reference records to {@link CardXref} instances. */
public final class CardXrefCodec {

    private static final RecordLayout LAYOUT = CardDemoLayouts.CARD_XREF;

    private CardXrefCodec() {
    }

    public static int recordLength() {
        return LAYOUT.recordLength();
    }

    public static CardXref decode(byte[] record) {
        return new CardXref(
                CardNumber.of(LAYOUT.rawText(record, "cardNumber")),
                LAYOUT.rawText(record, "customerId"),
                AccountId.of(LAYOUT.rawText(record, "accountId")));
    }
}
