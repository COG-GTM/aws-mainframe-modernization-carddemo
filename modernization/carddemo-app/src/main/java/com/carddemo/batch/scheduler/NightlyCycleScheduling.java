package com.carddemo.batch.scheduler;

import com.carddemo.batch.harness.BatchJobLauncher;
import java.time.Clock;
import org.springframework.batch.core.explore.JobExplorer;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;

/**
 * Turns on the cron trigger of {@code nightly-cycle} when {@code carddemo.batch.scheduler.enabled=true} and the app
 * runs as the web application. Never in a batch CLI process ({@code --job=...} runs without a web server), and off
 * in the {@code test} and {@code golden} profiles, so neither CI nor a parity run can be fired by the clock.
 */
@Configuration(proxyBeanMethods = false)
@ConditionalOnWebApplication
@ConditionalOnProperty(prefix = "carddemo.batch.scheduler", name = "enabled", havingValue = "true")
@EnableScheduling
public class NightlyCycleScheduling {

    @Bean
    NightlyCycleTrigger nightlyCycleTrigger(BatchJobLauncher launcher, JobExplorer explorer, Clock clock) {
        return new NightlyCycleTrigger(launcher, explorer, clock);
    }
}
