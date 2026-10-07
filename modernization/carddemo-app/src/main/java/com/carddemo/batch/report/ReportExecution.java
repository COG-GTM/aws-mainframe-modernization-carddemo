package com.carddemo.batch.report;

import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.List;

/**
 * One {@code report_request} row: an online CORPT00C submission of the {@code tranrept} job stream.
 *
 * @param executionId          {@code report_request_id}: the id the API returns and polls
 * @param jobExecutionIds      the Spring Batch job executions of the stream's steps, in step order (each has its
 *                             {@code batch_run} rows)
 * @param reportJobExecutionId the STEP15 (CBTRN03C) execution that wrote the TRANREPT generation
 * @param outputFileId         the {@code batch_output_file} row of that generation
 */
public record ReportExecution(long executionId, String jobStream, ReportName reportName, LocalDate startDate,
                              LocalDate endDate, LocalDate runDate, String encoding, String requestedBy,
                              ReportStatus status, Integer returnCode, List<Long> jobExecutionIds,
                              Long reportJobExecutionId, Long outputFileId, String message,
                              OffsetDateTime submittedAt, OffsetDateTime startedAt, OffsetDateTime endedAt) {
}
