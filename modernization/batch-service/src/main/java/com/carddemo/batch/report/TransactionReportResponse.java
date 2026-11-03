package com.carddemo.batch.report;

import java.time.Instant;
import java.time.LocalDate;

/** Status of a submitted report; {@code reportPath} is the TRANREPT file once COMPLETED. */
public record TransactionReportResponse(
        String reportId,
        ReportType reportType,
        TransactionReport.Status status,
        LocalDate startDate,
        LocalDate endDate,
        String submittedBy,
        Instant submittedAt,
        Instant startedAt,
        Instant completedAt,
        Long jobExecutionId,
        Integer returnCode,
        String reportPath,
        String message) {

    static TransactionReportResponse from(TransactionReport r) {
        boolean done = r.status() == TransactionReport.Status.COMPLETED;
        return new TransactionReportResponse(r.reportId(), r.reportType(), r.status(), r.startDate(), r.endDate(),
                r.submittedBy(), r.submittedAt(), r.startedAt(), r.completedAt(), r.jobExecutionId(), r.returnCode(),
                done && r.reportFile() != null ? r.reportFile().toString() : null, r.message());
    }
}
