package com.carddemo.interest.io.codec;

import com.carddemo.interest.domain.AccountId;
import com.carddemo.interest.domain.TransactionCategory;
import com.carddemo.interest.domain.TransactionCategoryBalance;
import com.carddemo.interest.io.RecordLayout;
import com.carddemo.interest.io.layout.CardDemoLayouts;

/** Maps {@code CVTRA01Y} records to {@link TransactionCategoryBalance} instances. */
public final class TransactionCategoryBalanceCodec {

    private static final RecordLayout LAYOUT = CardDemoLayouts.TRANSACTION_CATEGORY_BALANCE;

    private TransactionCategoryBalanceCodec() {
    }

    public static int recordLength() {
        return LAYOUT.recordLength();
    }

    public static TransactionCategoryBalance decode(byte[] record) {
        return new TransactionCategoryBalance(
                AccountId.of(LAYOUT.rawText(record, "accountId")),
                TransactionCategory.of(LAYOUT.rawText(record, "typeCode"),
                        LAYOUT.decimal(record, "categoryCode").intValueExact()),
                LAYOUT.decimal(record, "balance"));
    }

    public static byte[] encode(TransactionCategoryBalance balance) {
        byte[] record = LAYOUT.blankRecord();
        LAYOUT.putText(record, "accountId", balance.accountId().value());
        LAYOUT.putText(record, "typeCode", balance.category().typeCode());
        LAYOUT.putDecimal(record, "categoryCode", new java.math.BigDecimal(balance.category().categoryCode()));
        LAYOUT.putDecimal(record, "balance", balance.balance());
        return record;
    }
}
