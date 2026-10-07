package com.carddemo.batch.report;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Registers the {@link ReportExecutor} only in a web application with {@code carddemo.reports.async.enabled=true}
 * (default; false under {@code test} and {@code golden}, where report requests run on the calling thread, like
 * {@code NightlyCycleTrigger} is off there).
 */
@Configuration(proxyBeanMethods = false)
@ConditionalOnWebApplication
@ConditionalOnProperty(prefix = "carddemo.reports.async", name = "enabled", havingValue = "true",
        matchIfMissing = true)
public class ReportExecutorConfiguration {

    @Bean
    ReportExecutor reportExecutor(@Value("${carddemo.reports.async.queue-capacity:20}") int queueCapacity) {
        return new ReportExecutor(queueCapacity);
    }
}
