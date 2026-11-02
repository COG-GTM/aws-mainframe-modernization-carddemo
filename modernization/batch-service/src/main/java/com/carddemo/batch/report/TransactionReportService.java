package com.carddemo.batch.report;

import com.carddemo.batch.tranrept.TranreptJobConfig;
import com.carddemo.common.batch.BatchJobLauncher;
import com.carddemo.common.batch.BatchRun;
import com.carddemo.common.error.ForbiddenException;
import com.carddemo.common.error.NotFoundException;
import com.carddemo.common.security.CallerIdentity;
import com.carddemo.common.security.CallerTokenForwarder;
import jakarta.annotation.PreDestroy;
import java.time.Clock;
import java.time.LocalDate;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.BatchStatus;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;

/**
 * CORPT00C SUBMIT-JOB-TO-INTRDR in Java (decision d-report-trigger: in-process async job with polling).
 * The screen wrote the TRANREPT JCL to the internal reader and answered at once; here the request is
 * recorded, TRANREPT is queued on a single worker thread of this JVM (one initiator: runs are serialised)
 * and the caller gets the report id to poll. The run calls card/transaction services with the
 * submitter's bearer token.
 */
@Service
public class TransactionReportService {

    static final String NOT_FOUND = "Report not found...";
    static final String NOT_YOURS = "No access - report submitted by another user...";
    static final String INTERRUPTED = "Interrupted: batch-service restarted before the job finished";

    private static final Logger log = LoggerFactory.getLogger(TransactionReportService.class);

    private final TransactionReportRepository reports;
    private final BatchJobLauncher launcher;
    private final Clock clock;
    private final ExecutorService worker = Executors.newSingleThreadExecutor(r -> {
        Thread t = new Thread(r, "transaction-report");
        t.setDaemon(true);
        return t;
    });

    public TransactionReportService(TransactionReportRepository reports, BatchJobLauncher launcher, Clock clock) {
        this.reports = reports;
        this.launcher = launcher;
        this.clock = clock;
    }

    /** The CORPT00C edits on the request, then the submit. */
    public TransactionReport submit(TransactionReportRequest request, CallerIdentity caller, String authorization) {
        ReportDates.Range range = ReportDates.resolve(request, LocalDate.now(clock));
        TransactionReport report = new TransactionReport(UUID.randomUUID().toString(), request.reportType(),
                range.startDate(), range.endDate(), TransactionReport.Status.SUBMITTED, caller.userId(),
                clock.instant(), null, null, null, null, null, null);
        reports.insert(report);
        worker.execute(() -> run(report, authorization));
        return report;
    }

    /** Status of a report: its submitter or an admin. */
    public TransactionReport get(String reportId, CallerIdentity caller) {
        TransactionReport report = reports.find(reportId).orElseThrow(() -> new NotFoundException(NOT_FOUND));
        if (!caller.isAdmin() && !caller.userId().equals(report.submittedBy())) {
            throw new ForbiddenException(NOT_YOURS);
        }
        return report;
    }

    void run(TransactionReport report, String authorization) {
        reports.markRunning(report.reportId(), clock.instant());
        try {
            BatchRun run = CallerTokenForwarder.onBehalfOf(authorization,
                    () -> launcher.launch(TranreptJobConfig.arguments(report.startDate(), report.endDate())));
            boolean ok = run.execution().getStatus() == BatchStatus.COMPLETED;
            reports.markFinished(report.reportId(),
                    ok ? TransactionReport.Status.COMPLETED : TransactionReport.Status.FAILED, clock.instant(),
                    run.execution().getId(), run.returnCode(), run.outputDir(),
                    ok ? report.reportType().reportName() + " report completed"
                       : "TRANREPT ended " + run.execution().getExitStatus().getExitCode());
        } catch (Exception | Error ex) {
            log.error("TRANREPT for report {} failed", report.reportId(), ex);
            reports.markFinished(report.reportId(), TransactionReport.Status.FAILED, clock.instant(),
                    null, null, null, "Unable to run TRANREPT: " + ex.getClass().getSimpleName());
        }
    }

    /** Work queued in memory dies with the JVM; a restart reports it rather than leaving it RUNNING. */
    @EventListener(ApplicationReadyEvent.class)
    public void failInterruptedRequests() {
        int count = reports.failUnfinished(clock.instant(), INTERRUPTED);
        if (count > 0) {
            log.warn("Marked {} unfinished transaction report request(s) FAILED after restart", count);
        }
    }

    @PreDestroy
    void shutdown() {
        worker.shutdownNow();
    }
}
