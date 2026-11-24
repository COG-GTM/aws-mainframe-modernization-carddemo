package com.carddemo.transaction.batch.validation;

import com.carddemo.transaction.CardDemoProperties;
import com.carddemo.transaction.batch.ProgramBanner;
import com.carddemo.transaction.domain.DailyTransaction;
import com.carddemo.transaction.io.DailyTransactionFileReader;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.launch.support.RunIdIncrementer;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.FileSystemResource;
import org.springframework.transaction.PlatformTransactionManager;

/**
 * CBTRN01C as {@code DailyTransactionValidationJob}: DALYTRAN -> 2000-LOOKUP-XREF -> 3000-READ-ACCOUNT.
 */
@Configuration
public class DailyTransactionValidationJobConfig {

    public static final String JOB_NAME = "DailyTransactionValidationJob";

    @Bean(name = JOB_NAME)
    public Job dailyTransactionValidationJob(JobRepository jobRepository, Step dailyTransactionValidationStep) {
        return new JobBuilder(JOB_NAME, jobRepository)
                .incrementer(new RunIdIncrementer())
                .start(dailyTransactionValidationStep)
                .build();
    }

    @Bean
    public Step dailyTransactionValidationStep(JobRepository jobRepository, PlatformTransactionManager txManager,
                                               DailyTransactionFileReader validationDalytranReader,
                                               DailyTransactionValidationProcessor processor,
                                               ValidationSummaryWriter validationSummaryWriter,
                                               CardDemoProperties properties) {
        return new StepBuilder("CBTRN01C", jobRepository)
                .<DailyTransaction, ValidationResult>chunk(properties.batch().chunkSize(), txManager)
                .reader(validationDalytranReader)
                .processor(processor)
                .writer(validationSummaryWriter)
                .listener(validationSummaryWriter)
                .listener(new ProgramBanner("CBTRN01C"))
                .build();
    }

    @Bean
    @StepScope
    public DailyTransactionFileReader validationDalytranReader(CardDemoProperties properties,
                                                       @Value("#{jobParameters['dalytran']}") String dalytran) {
        // job parameter 'dalytran' overrides the configured DD DALYTRAN dataset
        return new DailyTransactionFileReader(new FileSystemResource(dalytran != null ? dalytran : properties.batch().dalytranFile()));
    }

    @Bean
    @StepScope
    public ValidationSummaryWriter validationSummaryWriter() {
        return new ValidationSummaryWriter();
    }
}
