package com.carddemo.account.domain;

/** Which COBOL paragraph a posting follows. Both share the idempotency ledger keyed by TRAN-ID. */
public enum PostingType {

    /** CBTRN02C 1500-B-LOOKUP-ACCT (reasons 101, 102, 103) and 2800-UPDATE-ACCOUNT-REC (balance and cycle buckets). */
    DAILY_TRANSACTION,

    /**
     * COBIL00C: {@code COMPUTE ACCT-CURR-BAL = ACCT-CURR-BAL - TRAN-AMT} then REWRITE. No credit
     * limit or expiration check and no cycle bucket update; only a missing account is rejected (101).
     */
    BILL_PAYMENT
}
