package com.carddemo.batch.core;

/** Ends a job with a legacy return code of 8, 12 or 16. */
public class JobFailure extends RuntimeException {

    private final int returnCode;

    public JobFailure(int returnCode, String message) {
        super(message);
        this.returnCode = returnCode;
    }

    public JobFailure(int returnCode, String message, Throwable cause) {
        super(message, cause);
        this.returnCode = returnCode;
    }

    public int returnCode() {
        return returnCode;
    }
}
