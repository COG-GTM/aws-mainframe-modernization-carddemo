package com.carddemo.common.batch;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.explore.JobExplorer;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.SimpleStepBuilder;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.transaction.PlatformTransactionManager;

/**
 * Builders that fix the batch shape of the target state (BATCH profile): one {@link Job} per JCL member
 * named after it, one step per {@code EXEC} named after the step, chunk-oriented with commit interval 1,
 * restartable. Restart identity is the {@code run.id} job parameter set by {@link JclJobLauncher}.
 */
public final class JclSteps {

    /** USER: "chunk de 1 con reinicio". One input record = one transaction (D-011). */
    public static final int COMMIT_INTERVAL = 1;

    private JclSteps() {
    }

    /** Job named after the JCL member; restartable, no incrementer (a new instance = a new {@code run.id}). */
    public static JobBuilder job(String jclMember, JobRepository jobRepository) {
        return new JobBuilder(jclMember, jobRepository);
    }

    /** Chunk step named after the JCL {@code EXEC} step, commit interval {@value #COMMIT_INTERVAL}. */
    public static <I, O> SimpleStepBuilder<I, O> chunkStep(String execStep, JobRepository jobRepository,
                                                          PlatformTransactionManager transactionManager) {
        return new StepBuilder(execStep, jobRepository).<I, O>chunk(COMMIT_INTERVAL, transactionManager);
    }

    /** True when {@code execution} restarts an instance that already ran (not a new instance). */
    public static boolean isRestart(JobExecution execution, JobExplorer jobExplorer) {
        return jobExplorer.getJobExecutions(execution.getJobInstance()).stream()
                .anyMatch(previous -> !previous.getId().equals(execution.getId()));
    }
}
