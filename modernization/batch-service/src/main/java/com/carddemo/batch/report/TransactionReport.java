package com.carddemo.batch.report;

import java.nio.file.Path;
import java.time.Instant;
import java.time.LocalDate;

/** One submitted CORPT00C report: a row of {@code batch.transaction_report_request}. */
public record TransactionReport(
        String reportId,
        ReportType reportType,
        LocalDate startDate,
        LocalDate endDate,
        Status status,
        String submittedBy,
        Instant submittedAt,
        Instant startedAt,
        Instant completedAt,
        Long jobExecutionId,
        Integer returnCode,
        Path outputDir,
        String message) {

    /** The JES-like life of the submitted job. */
    public enum Status { SUBMITTED, RUNNING, COMPLETED, FAILED }

    /** The TRANREPT report file (DD REPTFILE) of the run, once it completed. */
    public Path reportFile() {
        return outputDir == null ? null : outputDir.resolve("TRANREPT");
    }
}
