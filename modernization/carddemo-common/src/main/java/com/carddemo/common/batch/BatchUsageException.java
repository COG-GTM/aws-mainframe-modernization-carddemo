package com.carddemo.common.batch;

/** A launch that cannot start (bad arguments, unknown job, completed instance): exit {@link ReturnCode#JCL_ERROR}. */
public class BatchUsageException extends RuntimeException {

    public BatchUsageException(String message) {
        super(message);
    }

    public BatchUsageException(String message, Throwable cause) {
        super(message, cause);
    }
}
