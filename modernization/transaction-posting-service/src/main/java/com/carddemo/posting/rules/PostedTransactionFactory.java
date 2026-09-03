package com.carddemo.posting.rules;

import com.carddemo.mainframe.cobol.Db2TimestampFormatter;
import com.carddemo.posting.domain.DailyTransaction;
import com.carddemo.posting.domain.PostedTransaction;

/**
 * Turns a validated daily transaction into the record written to the transaction master.
 *
 * <p>Business rule BR-6, COBOL paragraph {@code 2000-POST-TRANSACTION}
 * ({@code app/cbl/CBTRN02C.cbl:424-438}): every field is carried over unchanged and only the
 * processing timestamp is new.
 */
public final class PostedTransactionFactory {

    private final Db2TimestampFormatter timestamps;

    public PostedTransactionFactory(Db2TimestampFormatter timestamps) {
        this.timestamps = timestamps;
    }

    /**
     * Stamps the transaction as posted.
     *
     * <p>The timestamp is taken per transaction, as {@code PERFORM Z-GET-DB2-FORMAT-TIMESTAMP}
     * inside the posting paragraph does ({@code app/cbl/CBTRN02C.cbl:437}), not once per run.
     */
    public PostedTransaction post(DailyTransaction transaction) {
        return new PostedTransaction(
                transaction.id(),
                transaction.typeCode(),
                transaction.categoryCode(),
                transaction.source(),
                transaction.description(),
                transaction.amount(),
                transaction.merchantId(),
                transaction.merchantName(),
                transaction.merchantCity(),
                transaction.merchantZip(),
                transaction.cardNumber(),
                transaction.originTimestamp(),
                timestamps.now());
    }
}
