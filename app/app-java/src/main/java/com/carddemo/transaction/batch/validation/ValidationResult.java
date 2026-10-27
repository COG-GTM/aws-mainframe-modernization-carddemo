package com.carddemo.transaction.batch.validation;

import com.carddemo.transaction.domain.DailyTransaction;

public record ValidationResult(DailyTransaction transaction, Outcome outcome) {

    public enum Outcome {
        /** XREF and ACCOUNT both found. */
        VERIFIED,
        /** 2000-LOOKUP-XREF INVALID KEY (WS-XREF-READ-STATUS = 4). */
        CARD_NOT_VERIFIED,
        /** 3000-READ-ACCOUNT INVALID KEY (WS-ACCT-READ-STATUS = 4). */
        ACCOUNT_NOT_FOUND
    }
}
