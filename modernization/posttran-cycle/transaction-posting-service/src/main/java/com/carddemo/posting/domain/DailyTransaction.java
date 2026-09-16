package com.carddemo.posting.domain;

import com.carddemo.recordio.codec.FixedWidthRecord;
import com.carddemo.recordio.codec.RecordEncoding;
import com.carddemo.recordio.layout.Transaction;
import com.carddemo.recordio.layout.TransactionLayout;

/**
 * One DALYTRAN record as read: the decoded fields and the 350-byte record area they came from.
 * CBTRN02C keeps both (DALYTRAN-RECORD is the FD area) and copies the raw area into the reject
 * record, so the image must travel with the decoded value.
 */
public record DailyTransaction(Transaction transaction, FixedWidthRecord image) {

    public static DailyTransaction decode(FixedWidthRecord image) {
        return new DailyTransaction(TransactionLayout.INSTANCE.decode(image), image);
    }

    public static DailyTransaction of(Transaction transaction, RecordEncoding encoding) {
        return new DailyTransaction(transaction, TransactionLayout.INSTANCE.encode(transaction, encoding));
    }
}
