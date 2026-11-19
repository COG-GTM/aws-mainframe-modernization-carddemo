package com.carddemo.transaction.batch;

import com.carddemo.common.record.CobolRecord;

/** One DALYTRAN record and its 1-based position in the input (the reject staging key). */
public record DailyTransaction(long inputSeq, CobolRecord record) {
}
