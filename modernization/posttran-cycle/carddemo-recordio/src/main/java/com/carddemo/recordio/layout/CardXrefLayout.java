package com.carddemo.recordio.layout;

import com.carddemo.recordio.codec.FixedWidthRecord;

/** {@code app/cpy/CVACT03Y.cpy}, RECLN 50. */
public final class CardXrefLayout implements RecordLayout<CardXref> {

    public static final CardXrefLayout INSTANCE = new CardXrefLayout();

    static final int CARD_NUM = 0;  // X(16)
    static final int CUST_ID = 16;  // 9(09)
    static final int ACCT_ID = 25;  // 9(11)
    static final int FILLER = 36;   // X(14)

    private CardXrefLayout() {
    }

    @Override
    public int length() {
        return CardXref.LENGTH;
    }

    @Override
    public CardXref decode(FixedWidthRecord r) {
        return new CardXref(r.text(CARD_NUM, 16), r.unsignedInt(CUST_ID, 9), r.text(ACCT_ID, 11));
    }

    @Override
    public void encodeInto(FixedWidthRecord r, CardXref x) {
        r.setText(CARD_NUM, 16, x.cardNumber());
        r.setUnsignedInt(CUST_ID, 9, x.customerId());
        r.setText(ACCT_ID, 11, x.accountId());
    }
}
