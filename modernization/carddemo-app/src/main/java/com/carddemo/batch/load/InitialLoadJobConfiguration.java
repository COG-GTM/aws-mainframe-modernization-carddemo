package com.carddemo.batch.load;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HexFormat;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.job.builder.SimpleJobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

/**
 * The {@code initial-load} job: the IDCAMS {@code DELETE}/{@code DEFINE}/{@code REPRO} setup jobs (ACCTFILE,
 * CARDFILE, CUSTFILE, XREFFILE, TRANFILE, TRANTYPE, TRANCATG, DISCGRP, TCATBALF, DUSRSECJ) plus the DALYTRAN input,
 * one step per {@link InitialLoadInput} through {@link VsamDatasetLoader}, then a {@code reconcile-counts} step that
 * fails the job unless every table holds exactly the rows its file supplied.
 *
 * <p>Job parameters: {@code source-dir}, {@code mode} ({@code REPLACE}/{@code UPSERT}) and {@code source-sha256}
 * (digest of the eleven files) identify the instance, so the same data is loaded once; add {@code run.id} to force
 * a reload. Per-dataset counts land in the job execution context as {@code <INPUT>.read|loaded|empty|rejects|rows}.
 */
@Configuration(proxyBeanMethods = false)
public class InitialLoadJobConfiguration {

    public static final String JOB_NAME = "initial-load";
    public static final String SOURCE_DIR = "source-dir";
    public static final String MODE = "mode";
    public static final String SOURCE_SHA256 = "source-sha256";

    private static final Logger log = LoggerFactory.getLogger(InitialLoadJobConfiguration.class);

    @Bean
    Job initialLoadJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                       VsamDatasetLoader loader, InitialLoadProperties properties) {
        List<Step> steps = new ArrayList<>();
        for (InitialLoadInput input : InitialLoadInput.values()) {
            steps.add(loadStep(input, jobRepository, transactionManager, loader, properties));
        }
        SimpleJobBuilder job = new JobBuilder(JOB_NAME, jobRepository)
                .start(clearStep(jobRepository, transactionManager, loader));
        steps.forEach(job::next);
        return job.next(reconcileStep(jobRepository, transactionManager, loader)).build();
    }

    /** Identifying parameters for loading {@code sourceDir} in {@code mode}. */
    public static JobParameters parameters(Path sourceDir, LoadMode mode) {
        return new JobParametersBuilder()
                .addString(SOURCE_DIR, sourceDir.toAbsolutePath().normalize().toString())
                .addString(MODE, mode.name())
                .addString(SOURCE_SHA256, digest(sourceDir))
                .toJobParameters();
    }

    /**
     * REPLACE mode: empties all eleven tables children-first in one transaction before the per-dataset steps load
     * parents before children. Otherwise a reload whose parent keys differ from the stored ones would fail the
     * (commit-time) foreign keys of children that are only replaced in a later step.
     */
    private static Step clearStep(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                                  VsamDatasetLoader loader) {
        return new StepBuilder("clear-tables", jobRepository).tasklet((contribution, chunk) -> {
            LoadMode mode = LoadMode.valueOf(chunk.getStepContext().getStepExecution().getJobParameters()
                    .getString(MODE));
            if (mode == LoadMode.REPLACE) {
                List<VsamDatasetLoader.Dataset> childrenFirst = new ArrayList<>();
                for (InitialLoadInput input : InitialLoadInput.values()) {
                    childrenFirst.add(input.dataset());
                }
                Collections.reverse(childrenFirst);
                loader.clear(childrenFirst);
                log.info("clear-tables: emptied {}", childrenFirst);
            }
            return RepeatStatus.FINISHED;
        }, transactionManager).build();
    }

    private static Step loadStep(InitialLoadInput input, JobRepository jobRepository,
                                 PlatformTransactionManager transactionManager, VsamDatasetLoader loader,
                                 InitialLoadProperties properties) {
        return new StepBuilder(input.stepName(), jobRepository).tasklet((contribution, chunk) -> {
            StepExecution step = chunk.getStepContext().getStepExecution();
            JobParameters params = step.getJobParameters();
            Path file = Path.of(params.getString(SOURCE_DIR)).resolve(input.fileName());
            LoadMode mode = LoadMode.valueOf(params.getString(MODE));
            LoadResult result = loader.load(input.dataset(), file, com.carddemo.common.codec.RecordEncoding.EBCDIC,
                    mode);
            for (int i = 0; i < result.read(); i++) {
                contribution.incrementReadCount();
            }
            for (int i = 0; i < result.rejects().size(); i++) {
                contribution.incrementReadSkipCount();
            }
            contribution.incrementFilterCount(result.empty());
            contribution.incrementWriteCount(result.loaded());
            ExecutionContext job = step.getJobExecution().getExecutionContext();
            job.putString(input + ".file", input.fileName());
            job.putLong(input + ".read", result.read());
            job.putLong(input + ".loaded", result.loaded());
            job.putLong(input + ".empty", result.empty());
            job.putLong(input + ".rejects", result.rejects().size());
            log.info("{} ({}): {} {} read={} loaded={} empty={} rejected={}", input.stepName(), input.jclJob(),
                    mode, input.fileName(), result.read(), result.loaded(), result.empty(),
                    result.rejects().size());
            result.rejects().forEach(r -> log.warn("{} record {} rejected: {}", input, r.recordNumber(),
                    r.reason()));
            if (result.rejects().size() > properties.maxRejects()) {
                throw new IllegalStateException(input + ": " + result.rejects().size()
                        + " rejected record(s), carddemo.initial-load.max-rejects=" + properties.maxRejects()
                        + "; first: record " + result.rejects().get(0).recordNumber() + " "
                        + result.rejects().get(0).reason());
            }
            return RepeatStatus.FINISHED;
        }, transactionManager).build();
    }

    private static Step reconcileStep(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                                      VsamDatasetLoader loader) {
        return new StepBuilder("reconcile-counts", jobRepository).tasklet((contribution, chunk) -> {
            StepExecution step = chunk.getStepContext().getStepExecution();
            LoadMode mode = LoadMode.valueOf(step.getJobParameters().getString(MODE));
            ExecutionContext job = step.getJobExecution().getExecutionContext();
            List<String> mismatches = new ArrayList<>();
            for (InitialLoadInput input : InitialLoadInput.values()) {
                long loaded = job.getLong(input + ".loaded");
                long rows = loader.count(input.dataset());
                job.putLong(input + ".rows", rows);
                boolean ok = mode == LoadMode.REPLACE ? rows == loaded : rows >= loaded;
                log.info("reconcile {}: file records={} loaded={} table rows={} {}", input,
                        job.getLong(input + ".read"), loaded, rows, ok ? "OK" : "MISMATCH");
                if (!ok) {
                    mismatches.add(input + " loaded " + loaded + " but table holds " + rows);
                }
            }
            if (!mismatches.isEmpty()) {
                throw new IllegalStateException("initial-load reconciliation failed: " + mismatches);
            }
            return RepeatStatus.FINISHED;
        }, transactionManager).build();
    }

    private static String digest(Path sourceDir) {
        try {
            MessageDigest sha = MessageDigest.getInstance("SHA-256");
            for (InitialLoadInput input : InitialLoadInput.values()) {
                Path file = sourceDir.resolve(input.fileName());
                sha.update(input.fileName().getBytes(java.nio.charset.StandardCharsets.US_ASCII));
                if (Files.isRegularFile(file)) {
                    sha.update(Files.readAllBytes(file));
                }
            }
            return HexFormat.of().formatHex(sha.digest());
        } catch (IOException e) {
            throw new UncheckedIOException("cannot hash initial-load inputs in " + sourceDir, e);
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException(e);
        }
    }
}
