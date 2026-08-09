package com.carddemo.posting.io.codec;

import com.carddemo.mainframe.io.RecordLayout;
import com.carddemo.mainframe.io.layout.CardDemoLayouts;
import com.carddemo.posting.domain.CardNumber;
import com.carddemo.posting.domain.DailyTransaction;

import java.math.BigDecimal;

/**
 * Maps {@code CVTRA06Y} daily-transaction records to {@link DailyTransaction} instances and back.
 *
 * <p>Encoding is exact: a decoded record re-encodes to the same 350 bytes, which is what lets a
 * rejected transaction be written out as "the input record plus a trailer"
 * ({@code app/cbl/CBTRN02C.cbl:447}) without carrying the raw bytes around in the domain model.
 */
public final class DailyTransactionCodec {

    private static final RecordLayout LAYOUT = CardDemoLayouts.DAILY_TRANSACTION;

    private DailyTransactionCodec() {
    }

    public static int recordLength() {
        return LAYOUT.recordLength();
    }

    public static DailyTransaction decode(byte[] record) {
        return new DailyTransaction(
                LAYOUT.rawText(record, "transactionId"),
                LAYOUT.rawText(record, "typeCode"),
                LAYOUT.rawText(record, "categoryCode"),
                LAYOUT.rawText(record, "source"),
                LAYOUT.rawText(record, "description"),
                LAYOUT.decimal(record, "amount"),
                LAYOUT.rawText(record, "merchantId"),
                LAYOUT.rawText(record, "merchantName"),
                LAYOUT.rawText(record, "merchantCity"),
                LAYOUT.rawText(record, "merchantZip"),
                CardNumber.of(LAYOUT.rawText(record, "cardNumber")),
                LAYOUT.rawText(record, "originTimestamp"),
                LAYOUT.rawText(record, "processTimestamp"));
    }

    public static byte[] encode(DailyTransaction transaction) {
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
