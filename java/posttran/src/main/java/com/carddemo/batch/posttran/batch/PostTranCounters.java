package com.carddemo.batch.posttran.batch;

/** WS-COUNTERS: WS-TRANSACTION-COUNT and WS-REJECT-COUNT (PIC 9(09)). */
public class PostTranCounters {

    private long transactionCount;
    private long rejectCount;

    void countTransaction() {
        transactionCount++;
    }

    void countReject() {
        rejectCount++;
    }

    public long transactionCount() {
        return transactionCount;
    }

    public long rejectCount() {
        return rejectCount;
    }
}
