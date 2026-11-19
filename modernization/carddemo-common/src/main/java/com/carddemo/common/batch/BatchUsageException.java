package com.carddemo.common.batch;

/** A launch rejected before any step runs (bad arguments, unknown job): exit {@link ReturnCode#JCL_ERROR}. */
public class BatchUsageException extends RuntimeException {

    public BatchUsageException(String message) {
        super(message);
    }
}
