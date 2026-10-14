package com.carddemo.batch.posttran.record;

import com.carddemo.batch.posttran.cobol.Pic;
import com.carddemo.batch.posttran.cobol.ZonedDecimal;

/**
 * DALYREJS record (LRECL 430): REJECT-TRAN-DATA X(350) = the DALYTRAN record, followed by
 * WS-VALIDATION-TRAILER = WS-VALIDATION-FAIL-REASON 9(04) + WS-VALIDATION-FAIL-REASON-DESC X(76).
 */
public final class RejectRecord {

    public static final int LENGTH = 430;

    private RejectRecord() {
    }

    public static String format(DailyTransaction tran, int reasonCode, String description) {
        return tran.raw() + ZonedDecimal.encodeUnsigned(reasonCode, 4) + Pic.x(description, 76);
    }
}
