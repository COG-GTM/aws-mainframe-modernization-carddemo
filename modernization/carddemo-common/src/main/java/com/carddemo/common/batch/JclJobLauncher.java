package com.carddemo.common.batch;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.JobParametersInvalidException;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.batch.core.repository.JobExecutionAlreadyRunningException;
import org.springframework.batch.core.repository.JobInstanceAlreadyCompleteException;
import org.springframework.batch.core.repository.JobRestartException;

import java.time.Clock;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collection;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;

/**
 * CLI launch seam (D-014): resolves {@code --job} to the Spring Batch {@link Job} of that name and runs it with
 * identifying parameters {@code run.id} and {@code dd.<DDNAME>}. Launching again with the {@code run.id} of a
 * FAILED execution restarts it from its last commit; a new {@code run.id} (generated when omitted) is a new
 * instance. Returns the JCL condition code (D-015).
 */
public class JclJobLauncher {

    public static final String RUN_ID = "run.id";
    public static final String DD_PREFIX = "dd.";

    private static final Logger log = LoggerFactory.getLogger(JclJobLauncher.class);
    private static final DateTimeFormatter RUN_ID_FORMAT = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss-SSS");

    private final Map<String, Job> jobs = new TreeMap<>();
    private final JobLauncher jobLauncher;
    private final Clock clock;

    public JclJobLauncher(Collection<Job> jobs, JobLauncher jobLauncher, Clock clock) {
        for (Job job : jobs) {
            this.jobs.put(job.getName().toUpperCase(Locale.ROOT), job);
        }
        this.jobLauncher = jobLauncher;
        this.clock = clock;
    }

    public int run(BatchArguments arguments) {
        Job job = jobs.get(arguments.job());
        if (job == null) {
            throw new BatchUsageException("Unknown job " + arguments.job() + "; registered: " + jobs.keySet());
        }
        String runId = arguments.runId() != null ? arguments.runId() : LocalDateTime.now(clock).format(RUN_ID_FORMAT);
        JobParametersBuilder parameters = new JobParametersBuilder().addString(RUN_ID, runId);
        arguments.dds().forEach((dd, path) -> parameters.addString(DD_PREFIX + dd, path));
        JobExecution execution = launch(job, parameters.toJobParameters());
        int rc = JclExitCodeMapper.map(execution);
        log.info("Job {} run.id={} execution={} status={} RC={}", job.getName(), runId, execution.getId(),
                execution.getStatus(), rc);
        return rc;
    }

    private JobExecution launch(Job job, JobParameters parameters) {
        try {
            return jobLauncher.run(job, parameters);
        } catch (JobParametersInvalidException ex) {
            throw new BatchUsageException(ex.getMessage(), ex);
        } catch (JobInstanceAlreadyCompleteException ex) {
            throw new BatchUsageException("Job " + job.getName() + " run.id=" + parameters.getString(RUN_ID)
                    + " already COMPLETED; use a new --run-id", ex);
        } catch (JobExecutionAlreadyRunningException | JobRestartException ex) {
            throw new BatchUsageException(ex.getMessage(), ex);
        }
    }
}
