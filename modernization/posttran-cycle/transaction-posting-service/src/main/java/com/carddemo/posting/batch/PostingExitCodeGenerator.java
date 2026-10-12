package com.carddemo.posting.batch;

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.launch.support.ExitCodeMapper;
import org.springframework.boot.ExitCodeGenerator;
import org.springframework.boot.autoconfigure.batch.JobExecutionEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.core.Ordered;

/**
 * Process exit code for the posting run, {@code CBTRN02C} lines 229-231
 * ({@code IF WS-REJECT-COUNT > 0 MOVE 4 TO RETURN-CODE}). Needed because Spring Boot's own
 * {@link org.springframework.boot.autoconfigure.batch.JobExecutionExitCodeGenerator} maps the job's
 * {@link org.springframework.batch.core.BatchStatus} ordinal and ignores the {@link org.springframework.batch.core.ExitStatus}
 * entirely, so {@code COMPLETED_WITH_REJECTS} would otherwise exit 0.
 */
public class PostingExitCodeGenerator implements ApplicationListener<JobExecutionEvent>, ExitCodeGenerator, Ordered {

    private final ExitCodeMapper mapper;
    private volatile JobExecution jobExecution;

    public PostingExitCodeGenerator(ExitCodeMapper mapper) {
        this.mapper = mapper;
    }

    @Override
    public void onApplicationEvent(JobExecutionEvent event) {
        this.jobExecution = event.getJobExecution();
    }

    @Override
    public int getExitCode() {
        JobExecution execution = this.jobExecution;
        if (execution == null) {
            return 0;
        }
        return this.mapper.intValue(execution.getExitStatus().getExitCode());
    }

    @Override
    public int getOrder() {
        return Ordered.HIGHEST_PRECEDENCE;
    }
}
