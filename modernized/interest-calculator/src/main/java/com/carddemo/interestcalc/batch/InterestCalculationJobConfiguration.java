package com.carddemo.interestcalc.batch;

import java.nio.file.Path;
import java.time.Clock;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

/**
 * Spring Batch wiring for the migrated CBACT04C job step.
 *
 * <p>The three job parameters are the modern form of the JCL: {@code runDate} is the
 * {@code PARM='2022071800'} of {@code app/jcl/INTCALC.jcl}, and the two directories stand in for
 * its DD statements.
 */
@Configuration
public class InterestCalculationJobConfiguration {

    public static final String JOB_NAME = "cbact04cInterestCalculationJob";
    public static final String STEP_NAME = "interestCalculationStep";

    @Bean
    public Job cbact04cInterestCalculationJob(JobRepository jobRepository, Step interestCalculationStep) {
        return new JobBuilder(JOB_NAME, jobRepository)
                .start(interestCalculationStep)
                .build();
    }

    @Bean
    public Step interestCalculationStep(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                                        InterestCalculationTasklet interestCalculationTasklet) {
        return new StepBuilder(STEP_NAME, jobRepository)
                .tasklet(interestCalculationTasklet, transactionManager)
                .build();
    }

    @Bean
    @StepScope
    public InterestCalculationTasklet interestCalculationTasklet(
            Clock clock,
            @Value("#{jobParameters['runDate']}") String runDate,
            @Value("#{jobParameters['inputDirectory']}") String inputDirectory,
            @Value("#{jobParameters['outputDirectory']}") String outputDirectory) {
        return new InterestCalculationTasklet(clock, runDate, Path.of(inputDirectory), Path.of(outputDirectory));
    }
}
