package com.carddemo.common.batch;

/**
 * JCL condition codes used by the CardDemo batch. The process exit code of a CLI launch is the job's
 * condition code: the highest step RC, {@link #ABEND} when a step fails, {@link #JCL_ERROR} when the
 * launch is rejected before any step runs.
 */
public final class ReturnCode {

    /** Normal end. */
    public static final int OK = 0;
    /** Normal end with warnings, e.g. CBTRN02C {@code MOVE 4 TO RETURN-CODE} when any record was rejected. */
    public static final int WARNING = 4;
    /** Usage error: bad arguments or unknown job (a JCL error; no step ran). */
    public static final int JCL_ERROR = 12;
    /** Abend ({@code CEE3ABD}, any unexpected FILE STATUS, any uncaught exception). */
    public static final int ABEND = 16;

    private ReturnCode() {
    }

    /** True for 0 and 4: the scheduler treats the run as successful. */
    public static boolean isSuccess(int returnCode) {
        return returnCode == OK || returnCode == WARNING;
    }

    /** Job RC = highest step RC. */
    public static int highest(int... returnCodes) {
        int max = OK;
        for (int rc : returnCodes) {
            validate(rc);
            max = Math.max(max, rc);
        }
        return max;
    }

    public static int validate(int returnCode) {
        if (returnCode < 0 || returnCode > 4095) {
            throw new IllegalArgumentException("RC must be 0-4095: " + returnCode);
        }
        return returnCode;
    }
}
