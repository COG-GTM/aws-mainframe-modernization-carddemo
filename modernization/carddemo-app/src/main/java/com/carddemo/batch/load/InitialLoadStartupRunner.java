package com.carddemo.batch.load;

import java.nio.file.Path;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.batch.core.repository.JobInstanceAlreadyCompleteException;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;

/**
 * Runs {@code initial-load} at startup when {@code carddemo.initial-load.on-startup=true} (the {@code local} profile,
 * i.e. {@code docker compose up}). Runs before the readiness probe flips, so the container is healthy only once the
 * data is in; a failed load stops the application. A completed load of the same files is skipped.
 */
@Component
@ConditionalOnProperty(prefix = "carddemo.initial-load", name = "on-startup", havingValue = "true")
class InitialLoadStartupRunner implements ApplicationRunner {

    private static final Logger log = LoggerFactory.getLogger(InitialLoadStartupRunner.class);

    private final JobLauncher jobLauncher;
    private final Job initialLoadJob;
    private final InitialLoadProperties properties;

    InitialLoadStartupRunner(JobLauncher jobLauncher, @Qualifier("initialLoadJob") Job initialLoadJob,
                             InitialLoadProperties properties) {
        this.jobLauncher = jobLauncher;
        this.initialLoadJob = initialLoadJob;
        this.properties = properties;
    }

    @Override
    public void run(ApplicationArguments args) throws Exception {
        Path sourceDir = properties.resolvedSourceDir();
        JobParameters parameters = InitialLoadJobConfiguration.parameters(sourceDir, properties.mode());
        JobExecution execution;
        try {
            execution = jobLauncher.run(initialLoadJob, parameters);
        } catch (JobInstanceAlreadyCompleteException e) {
            log.info("initial-load already completed for {} ({}), not reloading", sourceDir,
                    parameters.getString(InitialLoadJobConfiguration.SOURCE_SHA256));
            return;
        }
        if (execution.getStatus() != BatchStatus.COMPLETED) {
            throw new IllegalStateException("initial-load ended " + execution.getStatus() + ": "
                    + execution.getAllFailureExceptions());
        }
        log.info("initial-load completed from {} in {} mode", sourceDir, properties.mode());
    }
}
