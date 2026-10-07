package com.carddemo.batch.harness;

import java.time.Clock;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.core.Ordered;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;

/**
 * Runs the job named by {@code --job} (see {@link BatchCommandLine}) once the application has started, and hands
 * its RC to {@link BatchExitCodes} so {@code main} exits with it. Job parameters: {@code run-date} (a
 * {@link LocalDate}, default today on the injected clock), every other option as a string, and an identifying
 * {@code run.id} (epoch millis unless given, so each invocation is a new job instance; pass the failed run's
 * {@code --run.id} to restart it). Skipped when Boot's own runner is enabled ({@code spring.batch.job.enabled=true}).
 */
@Component
class BatchCommandLineRunner implements ApplicationRunner, Ordered {

    private static final Logger log = LoggerFactory.getLogger(BatchCommandLineRunner.class);

    private final BatchJobLauncher launcher;
    private final BatchExitCodes exitCodes;
    private final BatchRunLog runLog;
    private final List<CommandLineJobParameters> adapters;
    private final Clock clock;
    private final Environment environment;

    BatchCommandLineRunner(BatchJobLauncher launcher, BatchExitCodes exitCodes, BatchRunLog runLog,
                           List<CommandLineJobParameters> adapters, Clock clock, Environment environment) {
        this.launcher = launcher;
        this.exitCodes = exitCodes;
        this.runLog = runLog;
        this.adapters = adapters;
        this.clock = clock;
        this.environment = environment;
    }

    @Override
    public int getOrder() {
        return Ordered.LOWEST_PRECEDENCE;
    }

    @Override
    public void run(ApplicationArguments args) {
        if (environment.getProperty("spring.batch.job.enabled", Boolean.class, false)) {
            return;
        }
        Optional<BatchCommandLine.Request> request;
        try {
            request = BatchCommandLine.parse(args);
        } catch (IllegalArgumentException e) {
            log.error("batch CLI: {} (RC=0016)", e.getMessage());
            runLog.recordLaunchFailure(args.containsOption(BatchCommandLine.JOB)
                    ? String.valueOf(args.getOptionValues(BatchCommandLine.JOB)) : null, null, e.getMessage());
            exitCodes.add(ReturnCode.TERMINAL);
            return;
        }
        if (request.isEmpty()) {
            return;
        }
        JobOutcome outcome = run(request.get());
        exitCodes.add(outcome.returnCode());
    }

    JobOutcome run(BatchCommandLine.Request request) {
        JobParametersBuilder builder = new JobParametersBuilder()
                .addLocalDate(BatchCommandLine.RUN_DATE,
                        request.runDate() != null ? request.runDate() : LocalDate.now(clock));
        request.parameters().forEach((name, value) -> {
            if (!name.equals(BatchCommandLine.RUN_ID)) {
                builder.addString(name, value);
            }
        });
        String runId = request.parameters().get(BatchCommandLine.RUN_ID);
        builder.addLong(BatchCommandLine.RUN_ID, runId != null ? parseRunId(runId) : System.currentTimeMillis());
        JobParameters parameters = builder.toJobParameters();
        String jobName = launcher.job(request.jobName()).map(j -> j.getName()).orElse(request.jobName());
        try {
            for (CommandLineJobParameters adapter : adapters) {
                if (adapter.jobName().equals(jobName)) {
                    parameters = adapter.adapt(parameters);
                }
            }
        } catch (IllegalArgumentException e) {
            log.error("{}: {} (RC=0016)", jobName, e.getMessage());
            runLog.recordLaunchFailure(jobName, parameters, e.getMessage());
            return JobOutcome.notLaunched(jobName, e.getMessage());
        }
        log.info("batch CLI: launching {} with {}", jobName, parameters);
        return launcher.run(jobName, parameters);
    }

    private static long parseRunId(String runId) {
        try {
            return Long.parseLong(runId);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("--run.id must be a number, got '" + runId + "'", e);
        }
    }
}
