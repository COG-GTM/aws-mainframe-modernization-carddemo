package com.carddemo.report;

import java.time.Instant;
import java.time.LocalDate;
import java.util.UUID;

/** Body of the {@code carddemo-report-request} SQS message (messaging.md section 6). */
public record ReportRequest(
        String schemaVersion,
        UUID messageId,
        String reportType,
        LocalDate startDate,
        LocalDate endDate,
        String requestedBy,
        Instant requestedAt) {
}
