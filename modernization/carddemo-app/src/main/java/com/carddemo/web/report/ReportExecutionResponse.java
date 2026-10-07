package com.carddemo.web.report;

import com.carddemo.batch.report.ReportStatus;
import io.swagger.v3.oas.annotations.media.Schema;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.util.List;

/** Status of an online report execution, its {@code batch_run} job rows and, once COMPLETED, the report. */
@Schema(description = "Report execution status (poll until status is COMPLETED or FAILED)")
public record ReportExecutionResponse(
        @Schema(example = "1") long executionId,
        @Schema(description = "Job stream run (the batch CLI equivalent is --job=tranrept)", example = "tranrept")
        String jobStream,
        @Schema(example = "Custom") String reportName,
        @Schema(example = "2022-01-01") LocalDate startDate,
        @Schema(example = "2022-07-06") LocalDate endDate,
        @Schema(description = "Business date of the run (dates the TRANREPT generation)") LocalDate runDate,
        @Schema(example = "ADMIN001") String requestedBy,
        ReportStatus status,
        @Schema(description = "Max condition code of the stream (0/4/8/12/16)", example = "0") Integer returnCode,
        String message,
        OffsetDateTime submittedAt,
        OffsetDateTime startedAt,
        OffsetDateTime endedAt,
        @Schema(description = "batch_run job rows of the stream steps, in step order") List<JobRun> jobs,
        @Schema(description = "The produced report (COMPLETED and still retained)") Report report) {

    /** One {@code batch_run} job row. */
    public record JobRun(long jobExecutionId, String jobName, String status, String returnCode, long readCount,
                         long writeCount, LocalDateTime startTime, LocalDateTime endTime) {
    }

    /** The TRANREPT generation catalogued in {@code batch_output_file}. */
    public record Report(@Schema(example = "TRANREPT.2022-07-06.42") String fileName,
                         @Schema(description = "batch_output_file.output_file_id") long outputFileId,
                         long recordCount,
                         String sha256,
                         @Schema(description = "Encoding of the file as written (EBCDIC fixed or ASCII lines)")
                         String encoding,
                         @Schema(description = "Download of the exact file bytes",
                                 example = "/api/v1/reports/transactions/1/report") String downloadUrl,
                         @Schema(description = "Report lines decoded, trailing spaces removed") List<String> lines) {
    }
}
