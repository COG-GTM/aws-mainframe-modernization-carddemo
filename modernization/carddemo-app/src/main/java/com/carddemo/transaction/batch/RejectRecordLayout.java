package com.carddemo.transaction.batch;

import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.RecordLayout;

import java.math.BigDecimal;

/**
 * CBTRN02C FD-REJS-RECORD (DALYREJS, 430 bytes): the DALYTRAN record unchanged (FR-12) plus WS-VALIDATION-TRAILER,
 * {@code WS-VALIDATION-FAIL-REASON PIC 9(04)} and {@code WS-VALIDATION-FAIL-REASON-DESC PIC X(76)}
 * ({@code CBTRN02C.cbl:82-84,180-182,446-465}).
 */
public final class RejectRecordLayout {

    public static final int LENGTH = 430;

    public static final RecordLayout LAYOUT = RecordLayout.builder("DALYREJS", LENGTH)
            .alphanumeric("REJECT-RECORD", DailyTransactionLayout.LENGTH)
            .zoned("VALIDATION-FAIL-REASON", 4, 0, false)
            .alphanumeric("VALIDATION-FAIL-REASON-DESC", 76)
            .build();

    private RejectRecordLayout() {
    }

    /** The reject image in the input record's code page; the 350 input bytes are copied, never re-encoded. */
    public static CobolRecord of(CobolRecord dailyTransaction, int reason, String description) {
        CobolRecord reject = LAYOUT.blank(dailyTransaction.codePage())
                .setDecimal("VALIDATION-FAIL-REASON", BigDecimal.valueOf(reason))
                .setString("VALIDATION-FAIL-REASON-DESC", description);
        byte[] image = reject.bytes();
        System.arraycopy(dailyTransaction.bytes(), 0, image, 0, DailyTransactionLayout.LENGTH);
        return LAYOUT.wrap(image, dailyTransaction.codePage());
    }
}
