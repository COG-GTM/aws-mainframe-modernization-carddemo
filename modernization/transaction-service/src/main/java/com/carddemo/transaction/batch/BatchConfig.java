package com.carddemo.transaction.batch;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

/**
 * Spring Batch replacements for the two JCL jobs. Each job is a single tasklet that delegates to
 * the ported service, keeping the COBOL paragraph structure in one readable place.
 */
@Configuration
public class BatchConfig {

    public static final String POST_TRANSACTIONS_JOB = "postTransactionsJob";
    public static final String INTEREST_CALCULATION_JOB = "interestCalculationJob";

    /** JCL POSTTRAN, program CBTRN02C. */
    @Bean
    public Job postTransactionsJob(JobRepository jobRepository, Step postTransactionsStep) {
        return new JobBuilder(POST_TRANSACTIONS_JOB, jobRepository).start(postTransactionsStep).build();
    }

    @Bean
    public Step postTransactionsStep(JobRepository jobRepository,
                                     PlatformTransactionManager transactionManager,
                                     PostingService postingService) {
        return new StepBuilder("postTransactionsStep", jobRepository)
                .tasklet((contribution, chunkContext) -> {
                    PostingService.PostingReport report = postingService.postPendingTransactions();
                    chunkContext.getStepContext().getStepExecution().getExecutionContext()
                            .putInt("posted", report.posted());
                    chunkContext.getStepContext().getStepExecution().getExecutionContext()
                            .putInt("rejected", report.rejected());
                    return RepeatStatus.FINISHED;
                }, transactionManager)
                .build();
    }

    /** JCL INTCALC, program CBACT04C. */
    @Bean
    public Job interestCalculationJob(JobRepository jobRepository, Step interestCalculationStep) {
        return new JobBuilder(INTEREST_CALCULATION_JOB, jobRepository)
                .start(interestCalculationStep)
                .build();
    }

    @Bean
    public Step interestCalculationStep(JobRepository jobRepository,
                                        PlatformTransactionManager transactionManager,
                                        InterestCalculationService interestCalculationService) {
        return new StepBuilder("interestCalculationStep", jobRepository)
                .tasklet((contribution, chunkContext) -> {
                    InterestCalculationService.InterestReport report =
                            interestCalculationService.calculateInterest();
                    chunkContext.getStepContext().getStepExecution().getExecutionContext()
                            .putInt("accountsSettled", report.accountsSettled());
                    chunkContext.getStepContext().getStepExecution().getExecutionContext()
                            .putString("totalInterest", report.totalInterest().toPlainString());
                    return RepeatStatus.FINISHED;
                }, transactionManager)
                .build();
    }
}
