package com.carddemo.batch;

import com.carddemo.common.batch.BatchJobLauncher;
import com.carddemo.common.batch.BatchJobRegistry;
import java.nio.file.Path;
import java.time.Clock;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/** The jobs batch-service launches, keyed by JCL member. None yet: TRANREPT, CREASTMT, ... land here. */
@Configuration
public class BatchJobsConfig {

    @Bean
    public BatchJobRegistry batchJobRegistry() {
        return BatchJobRegistry.empty();
    }

    @Bean
    public BatchJobLauncher batchJobLauncher(JobLauncher jobLauncher, BatchJobRegistry registry,
                                             @Value("${carddemo.batch.output-dir:out}") Path outputDir,
                                             Clock clock) {
        return new BatchJobLauncher(jobLauncher, registry, outputDir, clock);
    }
}
