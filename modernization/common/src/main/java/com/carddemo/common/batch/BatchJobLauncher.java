package com.carddemo.common.batch;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobExecutionException;
import org.springframework.batch.core.launch.JobLauncher;

import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;

/**
 * Launches a registered job the same way for the CLI and the HTTP endpoints: a new output
 * generation, the job parameters, a synchronous run, and {@code RETURN-CODE} written next to the
 * outputs. Not a Spring component: services that run jobs declare it as a bean.
 */
public final class BatchJobLauncher {

    private final JobLauncher jobLauncher;
    private final BatchJobRegistry registry;
    private final Path defaultOutputRoot;
    private final Clock clock;

    public BatchJobLauncher(JobLauncher jobLauncher, BatchJobRegistry registry, Path defaultOutputRoot, Clock clock) {
        this.jobLauncher = jobLauncher;
        this.registry = registry;
        this.defaultOutputRoot = defaultOutputRoot;
        this.clock = clock;
    }

    public BatchJobRegistry registry() {
        return registry;
    }

    public BatchRun launch(BatchArguments args) throws JobExecutionException {
        Job job = registry.job(args.job());
        Path root = args.outputRoot() != null ? args.outputRoot() : defaultOutputRoot;
        Instant now = clock.instant();
        JobRunDirectory generation = JobRunDirectory.create(root, args.job(), now);
        JobExecution execution = jobLauncher.run(job, BatchJobParameters.of(args, generation.path(), now));
        int returnCode = ReturnCodes.exitCode(execution);
        generation.writeReturnCode(returnCode);
        return new BatchRun(args.job(), execution, generation.path(), returnCode);
    }
}
