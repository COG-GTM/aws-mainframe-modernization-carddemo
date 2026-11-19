package com.carddemo.app;

import com.carddemo.common.batch.JclJobLauncher;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import org.springframework.beans.factory.annotation.Value;

import java.time.Clock;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.List;

/** Wires the CLI launch seam over every {@link Job} bean (one per JCL member). */
@Configuration
public class BatchLaunchConfig {

    /**
     * The job clock (e.g. CBTRN02C {@code TRAN-PROC-TS}). {@code carddemo.batch.clock=yyyy-MM-ddTHH:mm:ss} pins it, the
     * equivalent of the oracle's {@code COB_CURRENT_DATE} (D-046); unset = system clock.
     */
    @Bean
    @ConditionalOnMissingBean
    Clock clock(@Value("${carddemo.batch.clock:}") String fixed) {
        if (fixed.isBlank()) {
            return Clock.systemDefaultZone();
        }
        ZoneId zone = ZoneId.systemDefault();
        return Clock.fixed(LocalDateTime.parse(fixed).atZone(zone).toInstant(), zone);
    }

    @Bean
    JclJobLauncher jclJobLauncher(List<Job> jobs, JobLauncher jobLauncher, Clock clock) {
        return new JclJobLauncher(jobs, jobLauncher, clock);
    }
}
