package com.carddemo.batch.core;

/** Legacy RETURN-CODE semantics (batch.md §1.1). */
public final class ReturnCode {

    public static final int OK = 0;
    public static final int WARNING = 4;
    public static final int INPUT_ERROR = 8;
    public static final int DATA_ERROR = 12;
    public static final int FATAL = 16;

    private ReturnCode() {
    }

    /** Container exit code: 0 for 0/4 (Batch job SUCCEEDED), the return code otherwise. */
    public static int processExitCode(int returnCode) {
        return returnCode <= WARNING ? 0 : returnCode;
    }
}
