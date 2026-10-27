package com.carddemo.modernization.batch;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.StepExecution;
import org.springframework.boot.ExitCodeGenerator;
import org.springframework.boot.autoconfigure.batch.JobExecutionEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.core.Ordered;

/**
 * Process exit code with JCL condition-code semantics: the highest step return code. Boot's own
 * {@code JobExecutionExitCodeGenerator} returns {@code BatchStatus.ordinal()}, which is 0 for a completed job with
 * rejects, so this generator runs first and is the one that decides.
 */
final class CycleExitCodeGenerator implements ExitCodeGenerator, ApplicationListener<JobExecutionEvent>, Ordered {
    static final int REJECTS = 4;
    static final int ABEND = 12;

    private final List<JobExecution> executions = new CopyOnWriteArrayList<>();

    @Override
    public void onApplicationEvent(JobExecutionEvent event) {
        executions.add(event.getJobExecution());
    }

    @Override
    public int getExitCode() {
        return executions.stream().mapToInt(CycleExitCodeGenerator::exitCode).max().orElse(0);
    }

    static int exitCode(JobExecution job) {
        if (job.getStatus() != BatchStatus.COMPLETED) {
            return ABEND;
        }
        return job.getStepExecutions().stream().mapToInt(CycleExitCodeGenerator::returnCode).max().orElse(0);
    }

    private static int returnCode(StepExecution step) {
        String code = step.getExitStatus().getExitCode();
        if (code.equals(ExitStatus.COMPLETED.getExitCode()) || code.equals(ExitStatus.NOOP.getExitCode())) {
            return 0;
        }
        return code.equals(PostingStepWriter.COMPLETED_WITH_REJECTS.getExitCode()) ? REJECTS : ABEND;
    }

    @Override
    public int getOrder() {
        return Ordered.HIGHEST_PRECEDENCE;
    }
}
