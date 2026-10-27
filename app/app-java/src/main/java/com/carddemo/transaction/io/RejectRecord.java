package com.carddemo.transaction.io;

import com.carddemo.transaction.cobol.Pic;

/**
 * REJECT-RECORD of DALYREJS (CBTRN02C): REJECT-TRAN-DATA X(350) + VALIDATION-TRAILER X(80), where the
 * trailer is WS-VALIDATION-FAIL-REASON 9(04) + WS-VALIDATION-FAIL-REASON-DESC X(76).
 */
public record RejectRecord(String tranData, int failReason, String failReasonDesc) {

    public static final int RECORD_LENGTH = 430;

    public String validationTrailer() {
        return Pic.zeroFill(failReason, 4) + Pic.x(failReasonDesc, 76);
    }

    public String format() {
        return Pic.x(tranData, 350) + validationTrailer();
    }
}
