package com.carddemo.batch.report;

/**
 * The report executor's queue is full: the request is recorded as FAILED and the API answers 503 (the TDQ
 * {@code WRITEQ TD} failure of CORPT00C R-19, {@code Unable to Write TDQ (JOBS)...}).
 */
public class ReportQueueFullException extends RuntimeException {

    private final long executionId;

    public ReportQueueFullException(long executionId, String message, Throwable cause) {
        super(message, cause);
        this.executionId = executionId;
    }

    public long executionId() {
        return executionId;
    }
}
