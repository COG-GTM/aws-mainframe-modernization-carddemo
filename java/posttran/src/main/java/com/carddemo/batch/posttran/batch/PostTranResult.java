package com.carddemo.batch.posttran.batch;

/** Outcome of one POSTTRAN run; {@code returnCode} is the job-step condition code. */
public record PostTranResult(int returnCode, long transactionCount, long rejectCount, Throwable abendCause) {

    public static final int RC_OK = 0;
    /** MOVE 4 TO RETURN-CODE when WS-REJECT-COUNT > 0. */
    public static final int RC_REJECTS = 4;
    /** 9999-ABEND-PROGRAM (CEE3ABD U999) has no RC on z/OS; the Java job ends FAILED with 12. */
    public static final int RC_ABEND = 12;

    public boolean abended() {
        return returnCode == RC_ABEND;
    }
}
