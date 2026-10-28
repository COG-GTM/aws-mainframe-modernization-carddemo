package com.carddemo.recordio.layout;

import com.carddemo.recordio.codec.FixedWidthRecord;

/**
 * {@code app/cpy/CVTRA05Y.cpy} (TRAN-RECORD) and {@code app/cpy/CVTRA06Y.cpy} (DALYTRAN-RECORD),
 * RECLN 350. Offsets are cumulative PIC lengths.
 */
public final class TransactionLayout implements RecordLayout<Transaction> {

    public static final TransactionLayout INSTANCE = new TransactionLayout();

    static final int ID = 0;            // X(16)
    static final int TYPE_CD = 16;      // X(02)
    static final int CAT_CD = 18;       // 9(04)
    static final int SOURCE = 22;       // X(10)
    static final int DESC = 32;         // X(100)
    static final int AMT = 132;         // S9(09)V99
    static final int MERCHANT_ID = 143; // 9(09)
    static final int MERCHANT_NAME = 152; // X(50)
    static final int MERCHANT_CITY = 202; // X(50)
    static final int MERCHANT_ZIP = 252;  // X(10)
    static final int CARD_NUM = 262;    // X(16)
    static final int ORIG_TS = 278;     // X(26)
    static final int PROC_TS = 304;     // X(26)
    static final int FILLER = 330;      // X(20)

    private TransactionLayout() {
    }

    @Override
    public int length() {
        return Transaction.LENGTH;
    }

    @Override
    public Transaction decode(FixedWidthRecord r) {
        return new Transaction(
                r.text(ID, 16),
                r.text(TYPE_CD, 2),
                (int) r.unsignedInt(CAT_CD, 4),
                r.text(SOURCE, 10),
                r.text(DESC, 100),
                r.zoned(AMT, 11, 2, true),
                r.unsignedInt(MERCHANT_ID, 9),
                r.text(MERCHANT_NAME, 50),
                r.text(MERCHANT_CITY, 50),
                r.text(MERCHANT_ZIP, 10),
                r.text(CARD_NUM, 16),
                r.text(ORIG_TS, 26),
                r.text(PROC_TS, 26));
    }

    @Override
    public void encodeInto(FixedWidthRecord r, Transaction t) {
        r.setText(ID, 16, t.id());
        r.setText(TYPE_CD, 2, t.typeCode());
        r.setUnsignedInt(CAT_CD, 4, t.categoryCode());
        r.setText(SOURCE, 10, t.source());
        r.setText(DESC, 100, t.description());
        r.setZoned(AMT, 11, 2, true, t.amount());
        r.setUnsignedInt(MERCHANT_ID, 9, t.merchantId());
        r.setText(MERCHANT_NAME, 50, t.merchantName());
        r.setText(MERCHANT_CITY, 50, t.merchantCity());
        r.setText(MERCHANT_ZIP, 10, t.merchantZip());
        r.setText(CARD_NUM, 16, t.cardNumber());
        r.setText(ORIG_TS, 26, t.originalTimestamp());
        r.setText(PROC_TS, 26, t.processingTimestamp());
    }
}
