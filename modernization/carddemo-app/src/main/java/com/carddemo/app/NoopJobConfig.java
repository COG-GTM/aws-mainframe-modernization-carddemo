package com.carddemo.app;

import com.carddemo.common.batch.JclSteps;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

/**
 * Job {@code NOOP}, step {@code STEP01}: reads nothing and ends RC 0. Smoke target of the Phase 0 exit gate
 * ({@code --job=NOOP}); it exercises the launch seam, the JobRepository and the chunk-1 step shape.
 */
@Configuration
public class NoopJobConfig {

    @Bean
    Job noopJob(JobRepository jobRepository, PlatformTransactionManager transactionManager) {
        return JclSteps.job("NOOP", jobRepository)
                .start(JclSteps.<Object, Object>chunkStep("STEP01", jobRepository, transactionManager)
                        .reader(() -> null)
                        .writer(chunk -> {
                        })
                        .build())
                .build();
    }
}
