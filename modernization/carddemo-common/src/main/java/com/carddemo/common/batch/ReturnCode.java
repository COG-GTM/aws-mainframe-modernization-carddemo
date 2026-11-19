package com.carddemo.common.batch;

/**
 * JCL condition codes used as process exit codes (D-015). 0 and 4 come from the COBOL source
 * ({@code CBTRN02C} sets {@code RETURN-CODE} 4 when a record was rejected, cbl:229-231); 16 replaces the
 * LE abend U999 ({@code CEE3ABD}, cbl:707-711); 12 is a launch/usage error with nothing processed.
 */
public final class ReturnCode {

    public static final int OK = 0;
    public static final int WARNING = 4;
    public static final int JCL_ERROR = 12;
    public static final int ABEND = 16;
    /** Highest condition code z/OS can report. */
    public static final int MAX = 4095;

    private ReturnCode() {
    }
}
