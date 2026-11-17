package com.carddemo.batch.load;

import java.nio.file.Path;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;
import org.springframework.core.env.Profiles;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.transaction.PlatformTransactionManager;

/**
 * {@code sampleDataLoadJob}: reloads every table from the sample files. The {@code dataDir} job parameter, or else
 * {@code BANK_CARDDEMO_SAMPLE_DATA_DIR}, points at the repository's {@code app/data} directory. The sample users have
 * public passwords, so the job refuses to run unless the {@code dev} or {@code test} profile is active or
 * {@code BANK_CARDDEMO_SAMPLE_DATA_ENABLED=true}.
 */
@Configuration(proxyBeanMethods = false)
public class SampleDataLoadJobConfiguration {

    public static final String JOB_NAME = "sampleDataLoadJob";

    public static final String DATA_DIR_PARAMETER = "dataDir";

    public static final String ENABLED_PROPERTY = "BANK_CARDDEMO_SAMPLE_DATA_ENABLED";

    private static final Logger log = LoggerFactory.getLogger(SampleDataLoadJobConfiguration.class);

    @Bean
    PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }

    @Bean
    Job sampleDataLoadJob(JobRepository jobRepository, Step loadSampleDataStep) {
        return new JobBuilder(JOB_NAME, jobRepository).start(loadSampleDataStep).build();
    }

    @Bean
    Step loadSampleDataStep(JobRepository jobRepository, PlatformTransactionManager transactionManager,
            SampleDataLoader loader, Environment environment,
            @Value("${BANK_CARDDEMO_SAMPLE_DATA_DIR:}") String defaultDataDir) {
        return new StepBuilder("loadSampleData", jobRepository)
                .tasklet((contribution, chunkContext) -> {
                    if (!isEnabled(environment)) {
                        throw new IllegalStateException(JOB_NAME + " seeds users with public sample passwords; activate"
                                + " the dev or test profile or set " + ENABLED_PROPERTY + "=true to run it");
                    }
                    String dataDir = contribution.getStepExecution().getJobParameters()
                            .getString(DATA_DIR_PARAMETER, defaultDataDir);
                    if (dataDir == null || dataDir.isBlank()) {
                        throw new IllegalStateException("Set the " + DATA_DIR_PARAMETER
                                + " job parameter or BANK_CARDDEMO_SAMPLE_DATA_DIR to the app/data directory");
                    }
                    Map<String, Integer> counts = loader.replaceAll(Path.of(dataDir));
                    counts.forEach((table, count) -> log.info("Loaded {} records into {}", count, table));
                    contribution.incrementWriteCount(counts.values().stream().mapToInt(Integer::intValue).sum());
                    return RepeatStatus.FINISHED;
                }, transactionManager)
                .build();
    }

    static boolean isEnabled(Environment environment) {
        return environment.acceptsProfiles(Profiles.of("dev", "test"))
                || environment.getProperty(ENABLED_PROPERTY, Boolean.class, false);
    }
}
