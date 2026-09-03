package com.carddemo.interest.io.codec;

import com.carddemo.interest.domain.InterestTransaction;
import com.carddemo.mainframe.io.RecordLayout;
import com.carddemo.mainframe.io.layout.CardDemoLayouts;

import java.math.BigDecimal;

/** Maps {@link InterestTransaction} instances onto {@code CVTRA05Y} output records. */
public final class TransactionCodec {

    private static final RecordLayout LAYOUT = CardDemoLayouts.TRANSACTION;

    private TransactionCodec() {
    }

    public static int recordLength() {
        return LAYOUT.recordLength();
    }

    public static byte[] encode(InterestTransaction transaction) {
        byte[] record = LAYOUT.blankRecord();
        LAYOUT.putText(record, "transactionId", transaction.transactionId());
        LAYOUT.putText(record, "typeCode", transaction.typeCode());
        LAYOUT.putDecimal(record, "categoryCode", BigDecimal.valueOf(transaction.categoryCode()));
        LAYOUT.putText(record, "source", transaction.source());
        LAYOUT.putText(record, "description", transaction.description());
        LAYOUT.putDecimal(record, "amount", transaction.amount());
        LAYOUT.putDecimal(record, "merchantId", new BigDecimal(transaction.merchantId()));
        LAYOUT.putText(record, "merchantName", transaction.merchantName());
        LAYOUT.putText(record, "merchantCity", transaction.merchantCity());
        LAYOUT.putText(record, "merchantZip", transaction.merchantZip());
        LAYOUT.putText(record, "cardNumber", transaction.cardNumber());
        LAYOUT.putText(record, "originTimestamp", transaction.originTimestamp());
        LAYOUT.putText(record, "processTimestamp", transaction.processTimestamp());
        return record;
    }
}
