package com.carddemo.batch.harness;

import java.time.LocalDate;
import java.time.LocalDateTime;

/** One {@code batch_run} row: a job run ({@code stepName == null}) or one of its step runs. */
public record BatchRun(long batchRunId, Long jobExecutionId, Long stepExecutionId, String jobName, String stepName,
                       LocalDate runDate, String status, String exitCode, ReturnCode returnCode, long readCount,
                       long writeCount, long skipCount, long filterCount, LocalDateTime startTime,
                       LocalDateTime endTime, String parameters, String message) {

    public boolean isJobRow() {
        return stepName == null;
    }
}
