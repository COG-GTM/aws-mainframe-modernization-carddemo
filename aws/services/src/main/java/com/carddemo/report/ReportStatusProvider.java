package com.carddemo.report;

import java.util.UUID;

public interface ReportStatusProvider {

    enum Status { SUBMITTED, RUNNING, SUCCEEDED, FAILED }

    record ReportStatus(Status status, String reportS3Key) {
    }

    /** Returns the status of the report execution named {@code requestId}; unknown executions are SUBMITTED. */
    ReportStatus status(UUID requestId);
}
