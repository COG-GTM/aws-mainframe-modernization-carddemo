package com.carddemo.batch.harness;

import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.JobExecution;

/**
 * How a job launch ended: its execution (null when it never started), status and condition code. {@code abended}
 * marks an abend or a launch failure (JCL error), after which a {@link JobChain} bypasses steps without
 * {@code COND=EVEN/ONLY}.
 */
public record JobOutcome(String jobName, JobExecution execution, BatchStatus status, ReturnCode returnCode,
                         boolean abended, String message) {

    public static JobOutcome of(JobExecution execution) {
        ReturnCode rc = ReturnCode.of(execution);
        return new JobOutcome(execution.getJobInstance().getJobName(), execution, execution.getStatus(), rc,
                ReturnCode.isAbend(execution.getAllFailureExceptions()), execution.getExitStatus().getExitDescription());
    }

    public static JobOutcome notLaunched(String jobName, String message) {
        return new JobOutcome(jobName, null, BatchStatus.ABANDONED, ReturnCode.TERMINAL, true, message);
    }

    public Long jobExecutionId() {
        return execution == null ? null : execution.getId();
    }

    public boolean launched() {
        return execution != null;
    }
}
