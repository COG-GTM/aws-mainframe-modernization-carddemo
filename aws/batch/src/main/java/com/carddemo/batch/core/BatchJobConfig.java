package com.carddemo.batch.core;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.batch.core.launch.support.TaskExecutorJobLauncher;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.repository.support.ResourcelessJobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.batch.support.transaction.ResourcelessTransactionManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Spring Batch infrastructure. Job metadata is not persisted in Spring Batch tables (the {@code carddemo} schema
 * is owned by the data-migration session); run state lives in {@code batch_job_run} (batch.md §4). Each job is
 * a single tasklet step that manages its own DB transactions (e.g. one per input record in POSTTRAN).
 */
@Configuration
public class BatchJobConfig {

    @Bean
    public JobRepository jobRepository() {
        return new ResourcelessJobRepository();
    }

    @Bean
    public JobLauncher jobLauncher(JobRepository jobRepository) throws Exception {
        TaskExecutorJobLauncher launcher = new TaskExecutorJobLauncher();
        launcher.setJobRepository(jobRepository);
        launcher.afterPropertiesSet();
        return launcher;
    }

    @Bean
    public JobCatalog jobCatalog(List<CardDemoJob> jobs, JobRepository jobRepository) {
        Map<String, CardDemoJob> byName = new LinkedHashMap<>();
        Map<String, Job> springJobs = new LinkedHashMap<>();
        ResourcelessTransactionManager noTx = new ResourcelessTransactionManager();
        for (CardDemoJob job : jobs) {
            if (byName.put(job.name(), job) != null) {
                throw new IllegalStateException("Duplicate job name " + job.name());
            }
            springJobs.put(job.name(), new JobBuilder(job.name(), jobRepository)
                    .start(new StepBuilder(job.name() + "-step", jobRepository)
                            .tasklet((contribution, chunkContext) -> {
                                JobExecutionHolder.execute(job);
                                return RepeatStatus.FINISHED;
                            }, noTx)
                            .build())
                    .build());
        }
        return new JobCatalog(byName, springJobs);
    }

    public record JobCatalog(Map<String, CardDemoJob> jobs, Map<String, Job> springJobs) {
    }
}
