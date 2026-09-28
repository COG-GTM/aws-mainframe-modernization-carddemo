package com.carddemo.report;

import java.util.UUID;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/** Local/no-op adapters used when no AWS integration is configured. */
@Configuration
public class LocalReportAdapters {

    private static final Logger LOG = LoggerFactory.getLogger(LocalReportAdapters.class);

    @Bean
    @ConditionalOnProperty(prefix = "carddemo.reports", name = "publisher", havingValue = "noop",
            matchIfMissing = true)
    public ReportPublisher noopReportPublisher() {
        return request -> LOG.info("Report request {} ({} {}..{}) accepted by no-op publisher",
                request.messageId(), request.reportType(), request.startDate(), request.endDate());
    }

    @Bean
    @ConditionalOnProperty(prefix = "carddemo.reports", name = "status", havingValue = "local",
            matchIfMissing = true)
    public ReportStatusProvider localReportStatusProvider() {
        return (UUID requestId) -> new ReportStatusProvider.ReportStatus(
                ReportStatusProvider.Status.SUBMITTED, null);
    }
}
