package com.carddemo.recordio.layout;

import com.carddemo.recordio.codec.FixedWidthRecord;

/** {@code app/cpy/CVTRA03Y.cpy}, RECLN 60. */
public final class TransactionTypeLayout implements RecordLayout<TransactionType> {

    public static final TransactionTypeLayout INSTANCE = new TransactionTypeLayout();

    static final int TYPE = 0;   // X(02)
    static final int DESC = 2;   // X(50)
    static final int FILLER = 52; // X(08)

    private TransactionTypeLayout() {
    }

    @Override
    public int length() {
        return TransactionType.LENGTH;
    }

    @Override
    public TransactionType decode(FixedWidthRecord r) {
        return new TransactionType(r.text(TYPE, 2), r.text(DESC, 50));
    }

    @Override
    public void encodeInto(FixedWidthRecord r, TransactionType t) {
        r.setText(TYPE, 2, t.typeCode());
        r.setText(DESC, 50, t.description());
    }
}
