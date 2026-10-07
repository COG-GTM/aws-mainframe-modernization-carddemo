package com.carddemo.batch.report;

import com.carddemo.batch.BatchOutputFile;
import com.carddemo.batch.BatchOutputFileRepository;
import com.carddemo.batch.harness.BatchCommandLine;
import com.carddemo.batch.harness.BatchJobLauncher;
import com.carddemo.batch.harness.BatchRun;
import com.carddemo.batch.harness.BatchRunLog;
import com.carddemo.batch.harness.DdParameters;
import com.carddemo.batch.harness.JobChain;
import com.carddemo.batch.harness.JobOutcome;
import com.carddemo.batch.harness.JobStream;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.tranrept.TranreptJobConfiguration;
import com.carddemo.common.codec.RecordEncoding;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.task.TaskRejectedException;
import org.springframework.stereotype.Service;

/**
 * Replaces CORPT00C's {@code WRITEQ TD QUEUE('JOBS')} of the TRNRPT00 JCL: records the request, then runs the same
 * {@code tranrept} job stream the batch CLI runs ({@code --job=tranrept}) on the report executor with
 * {@code PARM-START-DATE}/{@code PARM-END-DATE} = the validated window and all KSDS DDs on the tables. The report is
 * the TRANREPT generation STEP15 catalogues in {@code batch_output_file}, so the online and batch paths produce the
 * same bytes for the same window (ADR-0021).
 */
@Service
public class TransactionReportLauncher {

    /** Job parameter tagging the stream's executions with the request (also visible in {@code batch_run}). */
    public static final String REQUEST_PARAMETER = "report-request";
    public static final String MSG_QUEUE_FULL = "Unable to Write TDQ (JOBS)...";

    private static final Logger log = LoggerFactory.getLogger(TransactionReportLauncher.class);

    private final ReportRequestStore store;
    private final BatchJobLauncher launcher;
    private final List<JobStream> streams;
    private final BatchRunLog runLog;
    private final BatchOutputFileRepository files;
    private final ObjectProvider<ReportExecutor> executor;
    private final Clock clock;
    private final String encoding;

    public TransactionReportLauncher(ReportRequestStore store, BatchJobLauncher launcher, List<JobStream> streams,
                                     BatchRunLog runLog, BatchOutputFileRepository files,
                                     ObjectProvider<ReportExecutor> executor, Clock clock,
                                     @Value("${carddemo.reports.encoding:EBCDIC}") String encoding) {
        this.store = store;
        this.launcher = launcher;
        this.streams = streams;
        this.runLog = runLog;
        this.files = files;
        this.executor = executor;
        this.clock = clock;
        this.encoding = RecordEncoding.of(encoding) == RecordEncoding.EBCDIC ? "EBCDIC" : "ASCII";
    }

    /** Queues the stream for {@code window} and returns the QUEUED request (COMPLETED/FAILED without an executor). */
    public ReportExecution submit(ReportWindow window, String requestedBy) {
        LocalDate runDate = LocalDate.now(clock);
        long id = store.insert(TranreptJobConfiguration.TRANREPT, window, runDate, encoding, requestedBy);
        ReportExecutor async = executor.getIfAvailable();
        if (async == null) {
            run(id, window, runDate);
        } else {
            try {
                async.execute(() -> run(id, window, runDate));
            } catch (TaskRejectedException e) {
                store.finished(id, ReportStatus.FAILED, ReturnCode.TERMINAL.code(), List.of(), null, null,
                        MSG_QUEUE_FULL + " " + e.getMessage(), OffsetDateTime.now(clock));
                throw new ReportQueueFullException(id, MSG_QUEUE_FULL, e);
            }
        }
        log.info("report request {}: {} {}..{} by {} submitted", id, window.name().label(), window.startDate(),
                window.endDate(), requestedBy);
        return find(id).orElseThrow();
    }

    /** The job parameters of the stream for a request; {@code --job=tranrept} with the same window is equivalent. */
    JobParameters parameters(long id, ReportWindow window, LocalDate runDate) {
        return new JobParametersBuilder()
                .addLocalDate(BatchCommandLine.RUN_DATE, runDate)
                .addLong(BatchCommandLine.RUN_ID, clock.millis() ^ System.nanoTime())
                .addString(DdParameters.ENCODING, encoding)
                .addString(TranreptJobConfiguration.PARM_START_DATE, window.startDate().toString())
                .addString(TranreptJobConfiguration.PARM_END_DATE, window.endDate().toString())
                .addString(REQUEST_PARAMETER, String.valueOf(id))
                .toJobParameters();
    }

    void run(long id, ReportWindow window, LocalDate runDate) {
        store.running(id, OffsetDateTime.now(clock));
        List<Long> executions = new ArrayList<>();
        try {
            JobStream stream = streams.stream().filter(s -> s.name().equals(TranreptJobConfiguration.TRANREPT))
                    .findFirst().orElseThrow(() -> new IllegalStateException("tranrept job stream not registered"));
            JobChain.Result result = stream.chain(launcher, parameters(id, window, runDate)).run();
            JobOutcome report = null;
            for (JobChain.StepResult step : result.steps()) {
                if (step.outcome() != null && step.outcome().jobExecutionId() != null) {
                    executions.add(step.outcome().jobExecutionId());
                }
                if (step.step().stepName().equals(TranreptJobConfiguration.STEP15)) {
                    report = step.outcome();
                }
            }
            Long reportExecution = report == null ? null : report.jobExecutionId();
            Optional<BatchOutputFile> output = reportExecution == null ? Optional.empty()
                    : files.findFirstByGdgBaseAndJobExecutionId(TranreptJobConfiguration.TRANREPT_GDG,
                            reportExecution);
            ReturnCode rc = result.maxReturnCode();
            boolean ok = !result.abended() && !rc.isFailure() && output.isPresent();
            String message = ok ? "TRANREPT " + output.get().getRecordCount() + " lines"
                    : failure(result, report, output.isPresent());
            store.finished(id, ok ? ReportStatus.COMPLETED : ReportStatus.FAILED, rc.code(), executions,
                    reportExecution, output.map(BatchOutputFile::getOutputFileId).orElse(null), message,
                    OffsetDateTime.now(clock));
            log.info("report request {}: tranrept ended {}{} ({})", id, rc.label(), result.abended() ? " ABEND" : "",
                    message);
        } catch (RuntimeException e) {
            log.error("report request {}: tranrept failed", id, e);
            store.finished(id, ReportStatus.FAILED, ReturnCode.TERMINAL.code(), executions, null, null,
                    e.getClass().getSimpleName() + ": " + e.getMessage(), OffsetDateTime.now(clock));
        }
    }

    private static String failure(JobChain.Result result, JobOutcome report, boolean output) {
        if (report == null) {
            return "STEP15 (CBTRN03C) bypassed: max RC " + result.maxReturnCode().label()
                    + (result.abended() ? " ABEND" : "");
        }
        if (!output) {
            return "STEP15 (CBTRN03C) ended " + report.returnCode().label() + " without a TRANREPT generation"
                    + (report.message() == null || report.message().isBlank() ? "" : ": " + report.message());
        }
        return "tranrept ended " + result.maxReturnCode().label() + (result.abended() ? " ABEND" : "");
    }

    public Optional<ReportExecution> find(long id) {
        return store.find(id);
    }

    /** The {@code batch_run} job rows of the request's executions, in step order. */
    public List<BatchRun> jobRuns(ReportExecution execution) {
        List<BatchRun> rows = new ArrayList<>();
        for (Long jobExecutionId : execution.jobExecutionIds()) {
            runLog.findByJobExecutionId(jobExecutionId).stream().filter(BatchRun::isJobRow).forEach(rows::add);
        }
        return rows;
    }

    /** The catalogued TRANREPT generation of a COMPLETED request; empty once pruned (ADR-0012 retention). */
    public Optional<ReportFile> report(ReportExecution execution) {
        if (execution.outputFileId() == null) {
            return Optional.empty();
        }
        return files.findById(execution.outputFileId()).flatMap(file -> {
            Path path = Path.of(file.getFilePath());
            if (!Files.isRegularFile(path)) {
                return Optional.empty();
            }
            try {
                return Optional.of(new ReportFile(file, Files.readAllBytes(path),
                        RecordEncoding.of(execution.encoding())));
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
        });
    }
}
