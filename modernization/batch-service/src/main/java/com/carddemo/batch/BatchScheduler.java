package com.carddemo.batch;

import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

/**
 * In-process scheduling for local/docker use. On AWS the nightly chain (POSTTRAN -> INTCALC -> CREASTMT) runs from
 * EventBridge Scheduler + Step Functions instead and this is disabled (carddemo.batch.scheduler.enabled=false).
 */
@Component
@EnableScheduling
@ConditionalOnProperty(name = "carddemo.batch.scheduler.enabled", havingValue = "true", matchIfMissing = true)
public class BatchScheduler {

    private final TransactionReportService reports;

    public BatchScheduler(TransactionReportService reports) {
        this.reports = reports;
    }

    /** Replaces CORPT00C's WRITEQ TD to the internal reader: drain queued report requests. */
    @Scheduled(fixedDelayString = "${carddemo.batch.report-poll-interval:PT30S}")
    public void drainReportRequests() {
        reports.processSubmittedRequests();
    }
}
