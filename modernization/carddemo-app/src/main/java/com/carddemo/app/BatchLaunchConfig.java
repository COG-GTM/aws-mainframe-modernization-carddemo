package com.carddemo.app;

import com.carddemo.common.batch.JclJobLauncher;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.time.Clock;
import java.util.List;

/** Wires the CLI launch seam over every {@link Job} bean (one per JCL member). */
@Configuration
public class BatchLaunchConfig {

    @Bean
    @ConditionalOnMissingBean
    Clock clock() {
        return Clock.systemDefaultZone();
    }

    @Bean
    JclJobLauncher jclJobLauncher(List<Job> jobs, JobLauncher jobLauncher, Clock clock) {
        return new JclJobLauncher(jobs, jobLauncher, clock);
    }
}
