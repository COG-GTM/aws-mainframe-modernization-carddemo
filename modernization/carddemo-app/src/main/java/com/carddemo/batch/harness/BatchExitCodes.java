package com.carddemo.batch.harness;

import org.springframework.boot.ExitCodeGenerator;
import org.springframework.boot.autoconfigure.batch.JobExecutionEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Component;

/**
 * The process exit code of a batch launch: the highest RC of the jobs run by the CLI (or by Boot's runner when
 * {@code spring.batch.job.enabled=true}). Replaces Boot's {@code JobExecutionExitCodeGenerator}, which is only
 * created when no other {@link ExitCodeGenerator} exists, so {@code SpringApplication.exit} returns 0/4/8/12/16.
 */
@Component
public class BatchExitCodes implements ExitCodeGenerator, ApplicationListener<JobExecutionEvent> {

    private ReturnCode highest;

    public synchronized void add(ReturnCode rc) {
        highest = highest == null ? rc : highest.max(rc);
    }

    public synchronized ReturnCode highest() {
        return highest == null ? ReturnCode.OK : highest;
    }

    @Override
    public void onApplicationEvent(JobExecutionEvent event) {
        add(ReturnCode.of(event.getJobExecution()));
    }

    @Override
    public int getExitCode() {
        return highest().code();
    }
}
