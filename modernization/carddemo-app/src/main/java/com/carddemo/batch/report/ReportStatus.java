package com.carddemo.batch.report;

/** Lifecycle of an online report request ({@code report_request.status}). */
public enum ReportStatus {
    /** Accepted and waiting for the report executor. */
    QUEUED,
    /** The tranrept job stream is running. */
    RUNNING,
    /** The stream ended with RC 0 or 4 and catalogued a TRANREPT generation. */
    COMPLETED,
    /** The stream ended with RC 8 or more, abended, or wrote no report. */
    FAILED;

    public boolean finished() {
        return this == COMPLETED || this == FAILED;
    }
}
