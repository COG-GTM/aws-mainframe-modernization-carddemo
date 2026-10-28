package com.carddemo.recordio.layout;

import com.carddemo.recordio.codec.FixedWidthRecord;

/** {@code app/cpy/CVTRA04Y.cpy}, RECLN 60. */
public final class TransactionCategoryLayout implements RecordLayout<TransactionCategory> {

    public static final TransactionCategoryLayout INSTANCE = new TransactionCategoryLayout();

    static final int TYPE_CD = 0;  // X(02)
    static final int CAT_CD = 2;   // 9(04)
    static final int DESC = 6;     // X(50)
    static final int FILLER = 56;  // X(04)

    private TransactionCategoryLayout() {
    }

    @Override
    public int length() {
        return TransactionCategory.LENGTH;
    }

    @Override
    public TransactionCategory decode(FixedWidthRecord r) {
        return new TransactionCategory(r.text(TYPE_CD, 2), (int) r.unsignedInt(CAT_CD, 4), r.text(DESC, 50));
    }

    @Override
    public void encodeInto(FixedWidthRecord r, TransactionCategory c) {
        r.setText(TYPE_CD, 2, c.typeCode());
        r.setUnsignedInt(CAT_CD, 4, c.categoryCode());
        r.setText(DESC, 50, c.description());
    }
}
