package com.carddemo.batch.core;

import com.carddemo.batch.core.BatchJobConfig.JobCatalog;
import com.carddemo.batch.storage.ObjectNotFoundException;
import com.carddemo.batch.storage.ObjectStore;
import com.carddemo.batch.storage.S3Keys;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.nio.charset.StandardCharsets;
import java.time.Clock;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.HexFormat;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ThreadLocalRandom;
import java.util.regex.Pattern;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.jdbc.CannotGetJdbcConnectionException;
import org.springframework.stereotype.Component;
import software.amazon.awssdk.core.exception.SdkException;

/**
 * Entry point for one container run: parses {@code --job=…}, enforces {@code runId} idempotency via
 * {@code batch_job_run}, launches the Spring Batch job and publishes the run result (batch.md §1.1, §4).
 */
@Component
public class JobRunner {

    private static final Logger log = LoggerFactory.getLogger(JobRunner.class);
    private static final DateTimeFormatter RUN_ID_TS = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'")
            .withZone(ZoneOffset.UTC);

    private final JobCatalog catalog;
    private final JobLauncher launcher;
    private final BatchRunRepository runs;
    private final AdvisoryLock locks;
    private final ObjectStore store;
    private final ObjectMapper mapper;
    private final Clock clock;

    public JobRunner(JobCatalog catalog, JobLauncher launcher, BatchRunRepository runs, AdvisoryLock locks,
            ObjectStore store, ObjectMapper mapper, Optional<Clock> clock) {
        this.catalog = catalog;
        this.launcher = launcher;
        this.runs = runs;
        this.locks = locks;
        this.store = store;
        this.mapper = mapper;
        this.clock = clock.orElse(Clock.systemUTC());
    }

    public static final Pattern RUN_ID = Pattern.compile("[A-Za-z0-9_-]{1,40}");

    /** Runs the job selected by {@code args}; returns the logical return code (0/4/8/12/16). */
    public int run(String... args) {
        Map<String, String> params = parseArgs(args);
        String jobName = params.remove("job");
        if (jobName == null || !catalog.jobs().containsKey(jobName)) {
            log.error("Unknown or missing --job={}; known jobs: {}", jobName, catalog.jobs().keySet());
            return ReturnCode.INPUT_ERROR;
        }
        String runId = Optional.ofNullable(params.remove("runId")).filter(s -> !s.isBlank())
                .orElseGet(this::newRunId);
        if (!RUN_ID.matcher(runId).matches()) {
            log.error("--runId must match {} (batch_job_run.run_id VARCHAR(40), Batch job name)", RUN_ID);
            return ReturnCode.INPUT_ERROR;
        }
        LocalDate businessDate;
        try {
            businessDate = Optional.ofNullable(params.remove("businessDate")).filter(s -> !s.isBlank())
                    .map(LocalDate::parse).orElseGet(() -> LocalDate.now(clock.withZone(ZoneOffset.UTC)));
        } catch (DateTimeParseException e) {
            log.error("--businessDate must be yyyy-MM-dd", e);
            return ReturnCode.INPUT_ERROR;
        }
        MDC.put("runId", runId);
        MDC.put("jobName", jobName);
        MDC.put("businessDate", businessDate.toString());
        try {
            return execute(new JobParams(jobName, runId, businessDate, params));
        } finally {
            MDC.clear();
        }
    }

    /**
     * Holds the {@code (runId, job)} advisory lock for the whole run: a concurrent launch exits 16 without running
     * (Batch retries it later), while a retry after a dead attempt acquires the lock at once and resumes.
     */
    private int execute(JobParams p) {
        Instant started = clock.instant();
        Optional<AdvisoryLock.Lease> lease;
        try {
            lease = locks.tryAcquire("run:" + p.jobName(), p.runId());
        } catch (DataAccessException e) {
            log.error("Data store unavailable", e);
            return finishWithoutDb(p, ReturnCode.FATAL, e, started);
        }
        if (lease.isEmpty()) {
            log.error("runId {} is already running for {}: not started", p.runId(), p.jobName());
            return ReturnCode.FATAL;
        }
        try (AdvisoryLock.Lease held = lease.get()) {
            return executeLocked(p, started);
        }
    }

    private int executeLocked(JobParams p, Instant started) {
        try {
            Optional<BatchRunRepository.Run> existing = runs.find(p.runId(), p.jobName());
            if (existing.isPresent() && existing.get().businessDate() != null
                    && !existing.get().businessDate().equals(p.businessDate())) {
                log.error("runId {} was used for {} with businessDate {}, not {}: use a new runId", p.runId(),
                        p.jobName(), existing.get().businessDate(), p.businessDate());
                return ReturnCode.INPUT_ERROR;
            }
            if (existing.isPresent() && existing.get().params() != null
                    && !runs.sameParams(existing.get().params(), p.params())) {
                log.error("runId {} was used for {} with parameters {}, not {}: use a new runId", p.runId(),
                        p.jobName(), existing.get().params(), runs.paramsJson(p.params()));
                return ReturnCode.INPUT_ERROR;
            }
            if (!runs.start(p.runId(), p.jobName(), p.businessDate(), p.params())) {
                BatchRunRepository.Run previous = runs.find(p.runId(), p.jobName()).orElseThrow();
                int rc = previous.exitCode();
                log.info("runId {} already completed for {} with returnCode {}: no-op", p.runId(), p.jobName(), rc);
                boolean published = writeResult(p, rc, readCounts(previous.counts()),
                        "already completed (no-op)", started);
                return published ? rc : ReturnCode.FATAL;
            }
        } catch (DataAccessException e) {
            log.error("Data store unavailable", e);
            return finishWithoutDb(p, ReturnCode.FATAL, e, started);
        }

        JobOutcome outcome = launch(p);
        log.info("{} finished returnCode={} counts={}", p.jobName(), outcome.returnCode(), outcome.counts());
        try {
            runs.finish(p.runId(), p.jobName(), outcome.returnCode(), outcome.counts());
        } catch (DataAccessException e) {
            log.error("Could not record batch_job_run", e);
            outcome = JobOutcome.of(Math.max(outcome.returnCode(), ReturnCode.FATAL), outcome.counts(),
                    "batch_job_run update failed: " + e.getMessage());
        }
        if (!writeResult(p, outcome.returnCode(), outcome.counts(), outcome.message(), started)) {
            return ReturnCode.FATAL;
        }
        return outcome.returnCode();
    }

    private JobOutcome launch(JobParams p) {
        JobExecutionHolder.begin(p);
        try {
            launcher.run(catalog.springJobs().get(p.jobName()), new JobParametersBuilder()
                    .addString("runId", p.runId())
                    .addString("businessDate", p.businessDate().toString())
                    .addLong("launchedAt", System.nanoTime())
                    .toJobParameters());
            RuntimeException error = JobExecutionHolder.error();
            if (error != null) {
                return failure(error);
            }
            JobOutcome outcome = JobExecutionHolder.outcome();
            return outcome != null ? outcome : JobOutcome.of(ReturnCode.DATA_ERROR, Map.of(), "no outcome");
        } catch (Exception e) {
            return failure(e);
        } finally {
            JobExecutionHolder.clear();
        }
    }

    static JobOutcome failure(Exception e) {
        int rc = classify(e);
        log.error("Job failed with returnCode {}: {}", rc, e.getMessage(), e);
        Map<String, Object> counts = e instanceof JobFailureWithCounts f ? f.counts() : Map.of();
        return JobOutcome.of(rc, counts, e.getMessage());
    }

    static int classify(Throwable e) {
        if (e instanceof JobFailure f) {
            return f.returnCode();
        }
        if (e instanceof CannotGetJdbcConnectionException || e instanceof DataAccessResourceFailureException
                || e instanceof SdkException) {
            return ReturnCode.FATAL;
        }
        if (e instanceof ObjectNotFoundException || e instanceof IllegalArgumentException) {
            return ReturnCode.INPUT_ERROR;
        }
        return ReturnCode.DATA_ERROR;
    }

    private int finishWithoutDb(JobParams p, int rc, Exception e, Instant started) {
        writeResult(p, rc, Map.of(), e.getMessage(), started);
        return rc;
    }

    /**
     * Publishes {@code runs/<runId>/<job>.json}. On failure the caller exits 16: Batch retries the same
     * {@code runId}, and a completed run then republishes its recorded outcome without rerunning the job.
     */
    private boolean writeResult(JobParams p, int rc, Map<String, Object> counts, String message, Instant started) {
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("runId", p.runId());
        result.put("jobName", p.jobName());
        result.put("businessDate", p.businessDate().toString());
        result.put("returnCode", rc);
        result.put("status", rc <= ReturnCode.WARNING ? BatchRunRepository.COMPLETED : BatchRunRepository.FAILED);
        result.put("counts", counts);
        if (message != null) {
            result.put("message", message);
        }
        result.put("startedAt", started.toString());
        result.put("endedAt", clock.instant().toString());
        try {
            store.put(S3Keys.runResult(p.runId(), p.jobName()),
                    mapper.writerWithDefaultPrettyPrinter().writeValueAsBytes(result), "application/json");
            return true;
        } catch (Exception e) {
            log.error("Could not write run result {}", S3Keys.runResult(p.runId(), p.jobName()), e);
            return false;
        }
    }

    private Map<String, Object> readCounts(String json) {
        if (json == null) {
            return Map.of();
        }
        try {
            return mapper.readValue(json, new TypeReference<LinkedHashMap<String, Object>>() {
            });
        } catch (Exception e) {
            return Map.of();
        }
    }

    private String newRunId() {
        return RUN_ID_TS.format(clock.instant()) + "-"
                + HexFormat.of().toHexDigits(ThreadLocalRandom.current().nextInt());
    }

    static Map<String, String> parseArgs(String... args) {
        Map<String, String> params = new LinkedHashMap<>();
        for (String arg : args) {
            if (arg.startsWith("--") && arg.contains("=")) {
                int eq = arg.indexOf('=');
                params.put(arg.substring(2, eq), arg.substring(eq + 1));
            }
        }
        return params;
    }

    static byte[] utf8(String s) {
        return s.getBytes(StandardCharsets.UTF_8);
    }
}
