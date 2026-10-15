package com.carddemo.batch;

import java.time.LocalDate;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

/**
 * One Spring Batch job per legacy JCL job. Each job has a single step that calls the corresponding service; the
 * JCL return code is surfaced as the step exit description so orchestration (EventBridge Scheduler / Step
 * Functions) can apply the same COND= branching.
 */
@Configuration
public class BatchJobsConfig {

    /** Results by job execution id, for the REST API (execution contexts only hold the summary). */
    @Bean
    public Map<Long, JobResult> jobResults() {
        return new ConcurrentHashMap<>();
    }

    private Job job(String name, JobRepository repo, PlatformTransactionManager tx, Map<Long, JobResult> results,
                    Function<JobParameters, JobResult> body) {
        return new JobBuilder(name, repo)
                .start(new StepBuilder(name + "Step", repo).tasklet((contribution, chunk) -> {
                    var exec = chunk.getStepContext().getStepExecution();
                    JobResult r = body.apply(exec.getJobParameters());
                    results.put(exec.getJobExecutionId(), r);
                    exec.getExecutionContext().putString("counters", String.valueOf(r.counters()));
                    contribution.setExitStatus(new ExitStatus(ExitStatus.COMPLETED.getExitCode(),
                            "RC=" + r.returnCode()));
                    return RepeatStatus.FINISHED;
                }, tx).build())
                .build();
    }

    @Bean
    public Job posttran(JobRepository repo, PlatformTransactionManager tx, Map<Long, JobResult> jobResults,
                        PostTransactionsService service) {
        return job("POSTTRAN", repo, tx, jobResults, p -> service.run());
    }

    @Bean
    public Job intcalc(JobRepository repo, PlatformTransactionManager tx, Map<Long, JobResult> jobResults,
                       InterestCalculationService service) {
        return job("INTCALC", repo, tx, jobResults, p -> service.run(LocalDate.parse(p.getString("date"))));
    }

    @Bean
    public Job tranrept(JobRepository repo, PlatformTransactionManager tx, Map<Long, JobResult> jobResults,
                        TransactionReportService service) {
        return job("TRANREPT", repo, tx, jobResults, p -> service.run(LocalDate.parse(p.getString("startDate")),
                LocalDate.parse(p.getString("endDate"))));
    }

    @Bean
    public Job creastmt(JobRepository repo, PlatformTransactionManager tx, Map<Long, JobResult> jobResults,
                        StatementService service) {
        return job("CREASTMT", repo, tx, jobResults, p -> service.run());
    }
}
