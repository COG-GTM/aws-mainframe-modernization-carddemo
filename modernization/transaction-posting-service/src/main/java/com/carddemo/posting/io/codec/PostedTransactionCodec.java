package com.carddemo.posting.io.codec;

import com.carddemo.mainframe.io.RecordLayout;
import com.carddemo.mainframe.io.layout.CardDemoLayouts;
import com.carddemo.posting.domain.PostedTransaction;

import java.math.BigDecimal;

/**
 * Maps {@link PostedTransaction} instances onto {@code CVTRA05Y} transaction-master records.
 *
 * <p>Encoding is the Java equivalent of {@code WRITE FD-TRANFILE-REC FROM TRAN-RECORD}
 * ({@code app/cbl/CBTRN02C.cbl:564}).
 */
public final class PostedTransactionCodec {

    private static final RecordLayout LAYOUT = CardDemoLayouts.TRANSACTION;

    private PostedTransactionCodec() {
    }

    public static int recordLength() {
        return LAYOUT.recordLength();
    }

    public static byte[] encode(PostedTransaction transaction) {
        byte[] record = LAYOUT.blankRecord();
        LAYOUT.putText(record, "transactionId", transaction.id());
        LAYOUT.putText(record, "typeCode", transaction.typeCode());
        LAYOUT.putDecimal(record, "categoryCode", new BigDecimal(transaction.categoryCode()));
        LAYOUT.putText(record, "source", transaction.source());
        LAYOUT.putText(record, "description", transaction.description());
        LAYOUT.putDecimal(record, "amount", transaction.amount());
        LAYOUT.putDecimal(record, "merchantId", new BigDecimal(transaction.merchantId()));
        LAYOUT.putText(record, "merchantName", transaction.merchantName());
        LAYOUT.putText(record, "merchantCity", transaction.merchantCity());
        LAYOUT.putText(record, "merchantZip", transaction.merchantZip());
        LAYOUT.putText(record, "cardNumber", transaction.cardNumber().value());
        LAYOUT.putText(record, "originTimestamp", transaction.originTimestamp());
        LAYOUT.putText(record, "processTimestamp", transaction.processTimestamp());
        return record;
    }
}
