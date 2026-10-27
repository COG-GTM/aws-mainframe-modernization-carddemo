package com.carddemo.modernization.batch;

import com.carddemo.modernization.interest.InterestInput;
import com.carddemo.modernization.interest.InterestRun;
import com.carddemo.modernization.records.DailyTransactionRecord;
import java.nio.charset.Charset;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

/** One job, two steps in JCL order: POSTTRAN STEP15 (CBTRN02C), then INTCALC STEP15 (CBACT04C). */
@Configuration(proxyBeanMethods = false)
class DailyCycleJobConfig {

    @Bean
    Clock cycleClock(CycleProperties properties) {
        CycleProperties.ClockSettings settings = properties.clock();
        ZoneId zone = isBlank(settings.zone()) ? ZoneId.systemDefault() : ZoneId.of(settings.zone());
        return isBlank(settings.fixedInstant()) ? Clock.system(zone)
                : Clock.fixed(Instant.parse(settings.fixedInstant()), zone);
    }

    @Bean
    Charset cycleCharset(CycleProperties properties) {
        return Charset.forName(properties.encoding());
    }

    @Bean
    CycleExitCodeGenerator cycleExitCodeGenerator() {
        return new CycleExitCodeGenerator();
    }

    @Bean
    Job dailyCycleJob(JobRepository jobRepository, Step postingStep, Step interestStep, CycleProperties properties) {
        RunDirectory runDirectory = new RunDirectory(properties.runRoot());
        // Interest follows posting on COMPLETED and on COMPLETED_WITH_REJECTS (RC 4); only a failed step stops the job.
        return new JobBuilder("dailyCycleJob", jobRepository)
                .validator(runDirectory)
                .listener(runDirectory)
                .start(postingStep)
                .next(interestStep)
                .build();
    }

    @Bean
    Step postingStep(JobRepository jobRepository, PlatformTransactionManager transactionManager,
            CycleProperties properties, Clock cycleClock, Charset cycleCharset) {
        return new StepBuilder("postingStep", jobRepository)
                .<byte[], byte[]>chunk(properties.posting().chunkSize(), transactionManager)
                .reader(new FixedLengthRecordReader(properties.posting().dalytran(), DailyTransactionRecord.CODEC.length()))
                .writer(new PostingStepWriter(properties, cycleClock, cycleCharset))
                .build();
    }

    @Bean
    Step interestStep(JobRepository jobRepository, PlatformTransactionManager transactionManager,
            CycleProperties properties, Clock cycleClock, Charset cycleCharset) {
        return new StepBuilder("interestStep", jobRepository)
                .<InterestInput, InterestRun>chunk(1, transactionManager)
                .reader(new InterestInputReader(properties.interest(), cycleCharset))
                .processor(new InterestStepProcessor(properties, cycleClock))
                .writer(new InterestRunWriter(properties, cycleCharset))
                .build();
    }

    private static boolean isBlank(String value) {
        return value == null || value.isBlank();
    }
}
