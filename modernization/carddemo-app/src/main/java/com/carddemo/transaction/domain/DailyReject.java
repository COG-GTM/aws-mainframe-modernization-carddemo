package com.carddemo.transaction.domain;

import static com.carddemo.common.record.Copybooks.DALYREJS;

/** One DALYREJS record: DALYTRAN record verbatim + WS-VALIDATION-FAIL-REASON 9(04) + DESC X(76). */
public record DailyReject(DailyTransaction transaction, int reasonCode, String reasonDesc) {

    public String toRecord() {
        return DALYREJS.blank()
                .set("REJECT-TRAN-DATA", transaction.record())
                .set("WS-VALIDATION-FAIL-REASON", reasonCode)
                .set("WS-VALIDATION-FAIL-REASON-DESC", reasonDesc)
                .toString();
    }
}
