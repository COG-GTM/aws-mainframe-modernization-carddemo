package com.carddemo.common.record.layout;

import com.carddemo.common.record.AlphanumericField;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.NumericField;
import com.carddemo.common.record.RecordLayout;

import java.util.Arrays;

/**
 * {@code CBTRN02C} FD-REJS-RECORD: DALYREJS (daily transaction rejects, RECLN 430). There is no
 * copybook; the record is declared in the program: the 350-byte DALYTRAN record as read
 * ({@code FD-REJECT-RECORD}) followed by {@code WS-VALIDATION-TRAILER} (reason {@code 9(04)},
 * description {@code X(76)}) moved into {@code FD-VALIDATION-TRAILER}.
 */
public final class DalyrejsRecordLayout {

    private static final RecordLayout.Builder B = RecordLayout.builder("CBTRN02C", "FD-REJS-RECORD");

    public static final AlphanumericField FD_REJECT_RECORD = B.alpha("FD-REJECT-RECORD", "X(350)");
    public static final AlphanumericField FD_VALIDATION_TRAILER = B.group("FD-VALIDATION-TRAILER", 80);
    public static final NumericField WS_VALIDATION_FAIL_REASON = B.numeric("WS-VALIDATION-FAIL-REASON", "9(04)");
    public static final AlphanumericField WS_VALIDATION_FAIL_REASON_DESC =
            B.alpha("WS-VALIDATION-FAIL-REASON-DESC", "X(76)");
    public static final RecordLayout LAYOUT = B.build(430);

    private DalyrejsRecordLayout() {
    }

    /** {@code MOVE DALYTRAN-RECORD TO REJECT-TRAN-DATA} plus the validation trailer, as 2500-WRITE-REJECT-REC does. */
    public static CobolRecord of(CobolRecord dalytran, int reasonCode, String reasonDesc) {
        if (dalytran.layout() != DalytranRecordLayout.LAYOUT) {
            throw new IllegalArgumentException("Not a DALYTRAN record: " + dalytran.layout());
        }
        byte[] bytes = Arrays.copyOf(dalytran.toBytes(), LAYOUT.length());
        CobolRecord reject = CobolRecord.of(LAYOUT, dalytran.codePage(), bytes);
        reject.set(WS_VALIDATION_FAIL_REASON, reasonCode);
        reject.set(WS_VALIDATION_FAIL_REASON_DESC, reasonDesc);
        return reject;
    }

    /** The rejected DALYTRAN record carried in the first 350 bytes. */
    public static CobolRecord dalytran(CobolRecord reject) {
        byte[] bytes = Arrays.copyOf(reject.toBytes(), DalytranRecordLayout.LAYOUT.length());
        return CobolRecord.of(DalytranRecordLayout.LAYOUT, reject.codePage(), bytes);
    }
}
