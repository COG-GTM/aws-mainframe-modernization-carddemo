package com.carddemo.recordio.layout;

import com.carddemo.recordio.codec.FixedWidthRecord;

/** {@code app/cpy/CVTRA01Y.cpy}, RECLN 50. */
public final class TransactionCategoryBalanceLayout implements RecordLayout<TransactionCategoryBalance> {

    public static final TransactionCategoryBalanceLayout INSTANCE = new TransactionCategoryBalanceLayout();

    static final int ACCT_ID = 0;   // 9(11)
    static final int TYPE_CD = 11;  // X(02)
    static final int CAT_CD = 13;   // 9(04)
    static final int BAL = 17;      // S9(09)V99
    static final int FILLER = 28;   // X(22)

    private TransactionCategoryBalanceLayout() {
    }

    @Override
    public int length() {
        return TransactionCategoryBalance.LENGTH;
    }

    @Override
    public TransactionCategoryBalance decode(FixedWidthRecord r) {
        return new TransactionCategoryBalance(
                r.text(ACCT_ID, 11), r.text(TYPE_CD, 2), (int) r.unsignedInt(CAT_CD, 4), r.zoned(BAL, 11, 2, true));
    }

    @Override
    public void encodeInto(FixedWidthRecord r, TransactionCategoryBalance b) {
        r.setText(ACCT_ID, 11, b.accountId());
        r.setText(TYPE_CD, 2, b.typeCode());
        r.setUnsignedInt(CAT_CD, 4, b.categoryCode());
        r.setZoned(BAL, 11, 2, true, b.balance());
    }
}
