package com.carddemo.batch.harness;

import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.SortedSet;
import java.util.TreeSet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobExecutionException;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.stereotype.Component;

/**
 * Launches a job by name (case-insensitive, so {@code READACCT} finds {@code readacct}) and returns its
 * {@link JobOutcome} instead of throwing: a launch Spring Batch refuses (unknown job, invalid or already-completed
 * parameters) is recorded in {@code batch_run} as RC 16, like a JCL error.
 */
@Component
public class BatchJobLauncher {

    private static final Logger log = LoggerFactory.getLogger(BatchJobLauncher.class);

    private final JobLauncher jobLauncher;
    private final List<Job> jobs;
    private final BatchRunLog runLog;

    public BatchJobLauncher(JobLauncher jobLauncher, List<Job> jobs, BatchRunLog runLog) {
        this.jobLauncher = jobLauncher;
        this.jobs = List.copyOf(jobs);
        this.runLog = runLog;
    }

    public Optional<Job> job(String name) {
        String wanted = name.toLowerCase(Locale.ROOT);
        return jobs.stream().filter(j -> j.getName().toLowerCase(Locale.ROOT).equals(wanted)).findFirst();
    }

    public SortedSet<String> jobNames() {
        TreeSet<String> names = new TreeSet<>();
        jobs.forEach(j -> names.add(j.getName()));
        return names;
    }

    public JobOutcome run(String name, JobParameters parameters) {
        Optional<Job> job = job(name);
        if (job.isEmpty()) {
            return notLaunched(name, parameters, "unknown job '" + name + "'; known jobs: " + jobNames());
        }
        try {
            JobExecution execution = jobLauncher.run(job.get(), parameters);
            return JobOutcome.of(execution);
        } catch (JobExecutionException e) {
            return notLaunched(job.get().getName(), parameters, e.getClass().getSimpleName() + ": " + e.getMessage());
        }
    }

    private JobOutcome notLaunched(String name, JobParameters parameters, String message) {
        log.error("{} not launched (RC=0016): {}", name, message);
        runLog.recordLaunchFailure(name, parameters, message);
        return JobOutcome.notLaunched(name, message);
    }
}
