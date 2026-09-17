package com.carddemo.transaction.api;

import com.carddemo.transaction.batch.BatchConfig;
import java.util.Map;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * Replaces the JCL submit of POSTTRAN and INTCALC. A scheduler (EventBridge, Airflow, Control-M)
 * calls these endpoints on the same cadence the mainframe batch window used.
 */
@RestController
@RequestMapping("/api/v1/batch")
public class BatchJobController {

    private final JobLauncher jobLauncher;
    private final Job postTransactionsJob;
    private final Job interestCalculationJob;

    public BatchJobController(JobLauncher jobLauncher,
                              @Qualifier(BatchConfig.POST_TRANSACTIONS_JOB) Job postTransactionsJob,
                              @Qualifier(BatchConfig.INTEREST_CALCULATION_JOB) Job interestCalculationJob) {
        this.jobLauncher = jobLauncher;
        this.postTransactionsJob = postTransactionsJob;
        this.interestCalculationJob = interestCalculationJob;
    }

    @PostMapping("/post-transactions")
    public Map<String, Object> postTransactions() throws Exception {
        return summarize(jobLauncher.run(postTransactionsJob, runParameters()));
    }

    @PostMapping("/interest-calculation")
    public Map<String, Object> interestCalculation() throws Exception {
        return summarize(jobLauncher.run(interestCalculationJob, runParameters()));
    }

    private JobParameters runParameters() {
        return new JobParametersBuilder().addLong("runAt", System.currentTimeMillis()).toJobParameters();
    }

    private Map<String, Object> summarize(JobExecution execution) {
        return Map.of(
                "jobName", execution.getJobInstance().getJobName(),
                "executionId", execution.getId(),
                "status", execution.getStatus().name(),
                "results", execution.getStepExecutions().stream()
                        .flatMap(step -> step.getExecutionContext().entrySet().stream())
                        .collect(java.util.stream.Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue)));
    }
}
