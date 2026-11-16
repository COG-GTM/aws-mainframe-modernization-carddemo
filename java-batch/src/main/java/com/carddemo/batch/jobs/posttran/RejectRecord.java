package com.carddemo.batch.jobs.posttran;

import com.carddemo.batch.io.FixedWidthCodec;
import com.carddemo.batch.io.FieldWriter;
import com.carddemo.batch.model.DailyTransactionRecord;

/** DALYREJS record: the 350-byte DALYTRAN record plus the 80-byte validation trailer. */
public record RejectRecord(DailyTransactionRecord tran, int reason, String description) {

    public static final int LENGTH = 430;

    public String encode() {
        return DailyTransactionRecord.CODEC.encode(tran)
                + new FieldWriter(80).unsigned(reason, 4).text(description, 76).toString();
    }

    public static final FixedWidthCodec<RejectRecord> CODEC = FixedWidthCodec.of(LENGTH,
            r -> {
                throw new UnsupportedOperationException("DALYREJS is write-only");
            },
            RejectRecord::encode);
}
