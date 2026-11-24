package com.carddemo.transaction.batch;

import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.boot.ExitCodeGenerator;
import org.springframework.boot.autoconfigure.batch.JobExecutionEvent;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Maps the job outcome to the JCL step condition code: 0 = clean run, 4 = completed with rejects
 * (CBTRN02C {@code MOVE 4 TO RETURN-CODE}), 12 = abend.
 */
@Component
public class JclReturnCodes implements ExitCodeGenerator {

    public static final String COMPLETED_WITH_REJECTS = "COMPLETED WITH REJECTS";

    private volatile int exitCode;

    @EventListener
    public void onJobExecution(JobExecutionEvent event) {
        exitCode = Math.max(exitCode, returnCode(event.getJobExecution()));
    }

    public static int returnCode(JobExecution execution) {
        if (execution.getStatus() != BatchStatus.COMPLETED) {
            return 12;
        }
        return COMPLETED_WITH_REJECTS.equals(execution.getExitStatus().getExitCode()) ? 4 : 0;
    }

    public static ExitStatus completedWithRejects() {
        return new ExitStatus(COMPLETED_WITH_REJECTS);
    }

    @Override
    public int getExitCode() {
        return exitCode;
    }
}
