package com.carddemo.posting.io.codec;

import com.carddemo.mainframe.io.RecordLayout;
import com.carddemo.mainframe.io.layout.CardDemoLayouts;
import com.carddemo.posting.domain.AccountId;
import com.carddemo.posting.domain.TransactionCategoryBalance;
import com.carddemo.posting.domain.TransactionCategoryKey;

import java.math.BigDecimal;

/**
 * Maps {@code CVTRA01Y} transaction-category-balance records to
 * {@link TransactionCategoryBalance} instances and back.
 */
public final class TransactionCategoryBalanceCodec {

    private static final RecordLayout LAYOUT = CardDemoLayouts.TRANSACTION_CATEGORY_BALANCE;

    private TransactionCategoryBalanceCodec() {
    }

    public static int recordLength() {
        return LAYOUT.recordLength();
    }

    public static TransactionCategoryBalance decode(byte[] record) {
        return new TransactionCategoryBalance(
                new TransactionCategoryKey(
                        AccountId.of(LAYOUT.rawText(record, "accountId")),
                        LAYOUT.rawText(record, "typeCode"),
                        LAYOUT.rawText(record, "categoryCode")),
                LAYOUT.decimal(record, "balance"));
    }

    /** Encodes a balance into a fresh, blank record. */
    public static byte[] encode(TransactionCategoryBalance balance) {
        return encodeInto(LAYOUT.blankRecord(), balance);
    }

    /**
     * Encodes a balance over an existing record image.
     *
     * <p>Both the {@code REWRITE} of an existing bucket ({@code app/cbl/CBTRN02C.cbl:528}) and the
     * {@code WRITE} of a newly created one ({@code app/cbl/CBTRN02C.cbl:510}) emit the whole
     * record area, and {@code INITIALIZE TRAN-CAT-BAL-RECORD}
     * ({@code app/cbl/CBTRN02C.cbl:504}) leaves {@code FILLER PIC X(22)} untouched — so in both
     * cases the 22 filler bytes are whatever the record area last held. Encoding over an image
     * rather than over a blank record reproduces that.
     */
    public static byte[] encodeInto(byte[] recordImage, TransactionCategoryBalance balance) {
        byte[] record = recordImage.clone();
        TransactionCategoryKey key = balance.key();
        LAYOUT.putText(record, "accountId", key.accountId().value());
        LAYOUT.putText(record, "typeCode", key.typeCode());
        LAYOUT.putDecimal(record, "categoryCode", new BigDecimal(key.categoryCode()));
        LAYOUT.putDecimal(record, "balance", balance.balance());
        return record;
    }
}
