package com.carddemo.web.report;

import com.carddemo.batch.harness.BatchRun;
import com.carddemo.batch.report.ReportExecution;
import com.carddemo.batch.report.ReportFile;
import com.carddemo.batch.report.ReportQueueFullException;
import com.carddemo.batch.report.ReportStatus;
import com.carddemo.batch.report.ReportWindow;
import com.carddemo.batch.report.TransactionReportLauncher;
import com.carddemo.batch.report.TransactionReportService;
import com.carddemo.common.RecordNotFoundException;
import com.carddemo.common.web.ApiError;
import com.carddemo.common.web.ApiErrors;
import com.carddemo.user.UserType;
import com.carddemo.web.NavigationContext;
import com.carddemo.web.OpenApiConfiguration;
import com.carddemo.web.ScreenHeaders;
import com.carddemo.web.security.CurrentUser;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import java.net.URI;
import java.util.List;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.http.ContentDisposition;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ProblemDetail;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * CORPT00C (CR00, main menu option 9, any signed-on user). Instead of writing the TRNRPT00 JCL to the {@code JOBS}
 * TDQ, a confirmed request runs the {@code tranrept} job stream asynchronously and returns an execution id that
 * {@code GET /{executionId}} polls (ADR-0021). A USER sees only the executions they submitted (404 otherwise).
 */
@RestController
@ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
@RequestMapping(TransactionReportController.PATH)
@Tag(name = "Reports", description = "Transaction report submission and polling (CORPT00C -> tranrept stream)")
@SecurityRequirement(name = OpenApiConfiguration.BEARER)
public class TransactionReportController {

    public static final String PATH = "/api/v1/reports/transactions";
    public static final String TRAN_ID = "CR00";
    public static final String PROGRAM = "CORPT00C";
    public static final String MSG_EXECUTION_NOT_FOUND = "Report execution NOT found...";
    public static final String MSG_REPORT_NOT_AVAILABLE = "Report NOT available: execution is ";
    public static final String MSG_REPORT_NOT_RETAINED = "Report NOT available: generation no longer retained...";

    private static final String PROBLEM = "application/problem+json";

    private final TransactionReportService reports;
    private final TransactionReportLauncher launcher;
    private final ScreenHeaders headers;

    public TransactionReportController(TransactionReportService reports, TransactionReportLauncher launcher,
                                       ScreenHeaders headers) {
        this.reports = reports;
        this.launcher = launcher;
        this.headers = headers;
    }

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Request a transaction report (CORPT00C ENTER)",
            description = "MONTHLY = current month, YEARLY = current year, CUSTOM = the typed range (presence, "
                    + "NUMVAL-C normalisation, month/day ranges, CSUTLDTC calendar check, start <= end). confirm "
                    + "blank = validate and ask (200 VALIDATED), N = clear (200 CANCELLED), Y = queue the tranrept "
                    + "job stream with PARM-START-DATE/PARM-END-DATE and answer 202 SUBMITTED with the execution id.")
    @ApiResponse(responseCode = "200", description = "VALIDATED or CANCELLED",
            content = @Content(schema = @Schema(implementation = TransactionReportResponse.class)))
    @ApiResponse(responseCode = "202", description = "SUBMITTED: poll statusUrl (Location header)",
            content = @Content(schema = @Schema(implementation = TransactionReportResponse.class)))
    @ApiResponse(responseCode = "400", description = "INVREQ: no report type, a date edit failed, start after "
            + "end, or confirm other than Y/N",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    @ApiResponse(responseCode = "503", description = "NOSPACE: the report queue is full (R-19)",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    public ResponseEntity<TransactionReportResponse> submit(@AuthenticationPrincipal Jwt jwt,
                                                            @Valid @RequestBody TransactionReportRequest request) {
        TransactionReportService.Outcome outcome = reports.request(request.reportType(),
                request.startDate() == null ? null : request.startDate().toDomain(),
                request.endDate() == null ? null : request.endDate().toDomain(), request.confirm(),
                CurrentUser.of(jwt).userId());
        ReportWindow window = outcome.window();
        ReportExecution execution = outcome.execution();
        String statusUrl = execution == null ? null : PATH + "/" + execution.executionId();
        TransactionReportResponse body = new TransactionReportResponse(headers.of(TRAN_ID, PROGRAM),
                outcome.state(), window == null ? null : window.name().label(),
                window == null ? null : ReportDateFields.of(window.start()),
                window == null ? null : ReportDateFields.of(window.end()),
                window == null ? null : window.startDate(), window == null ? null : window.endDate(),
                execution == null ? null : execution.executionId(), execution == null ? null : execution.status(),
                statusUrl, outcome.message(), exit());
        if (execution != null) {
            return ResponseEntity.accepted().location(URI.create(statusUrl)).body(body);
        }
        return ResponseEntity.ok(body);
    }

    @GetMapping("/{executionId}")
    @Operation(summary = "Poll a report execution",
            description = "Status from report_request and the batch_run rows of the stream's jobs; once COMPLETED "
                    + "the TRANREPT generation catalogued in batch_output_file, decoded line by line.")
    @ApiResponse(responseCode = "200", description = "QUEUED, RUNNING, COMPLETED or FAILED",
            content = @Content(schema = @Schema(implementation = ReportExecutionResponse.class)))
    @ApiResponse(responseCode = "404", description = "NOTFND: no such execution (or submitted by another user)",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    public ReportExecutionResponse status(@AuthenticationPrincipal Jwt jwt,
                                          @Parameter(description = "Execution id returned by the submission")
                                          @PathVariable long executionId) {
        ReportExecution execution = visible(jwt, executionId);
        List<ReportExecutionResponse.JobRun> jobs = launcher.jobRuns(execution).stream()
                .map(TransactionReportController::jobRun).toList();
        ReportExecutionResponse.Report report = execution.status() == ReportStatus.COMPLETED
                ? launcher.report(execution).map(f -> report(execution, f)).orElse(null) : null;
        return new ReportExecutionResponse(execution.executionId(), execution.jobStream(),
                execution.reportName().label(), execution.startDate(), execution.endDate(), execution.runDate(),
                execution.requestedBy(), execution.status(), execution.returnCode(), execution.message(),
                execution.submittedAt(), execution.startedAt(), execution.endedAt(), jobs, report);
    }

    @GetMapping(path = "/{executionId}/report", produces = MediaType.APPLICATION_OCTET_STREAM_VALUE)
    @Operation(summary = "Download the produced report file",
            description = "The exact bytes of the TRANREPT generation (the same file --job=tranrept writes).")
    @ApiResponse(responseCode = "200", description = "The report file",
            content = @Content(mediaType = MediaType.APPLICATION_OCTET_STREAM_VALUE))
    @ApiResponse(responseCode = "404", description = "NOTFND: no such execution, not COMPLETED, or pruned",
            content = @Content(mediaType = PROBLEM, schema = @Schema(implementation = ApiError.class)))
    public ResponseEntity<byte[]> download(@AuthenticationPrincipal Jwt jwt, @PathVariable long executionId) {
        ReportExecution execution = visible(jwt, executionId);
        if (execution.status() != ReportStatus.COMPLETED) {
            throw new RecordNotFoundException(MSG_REPORT_NOT_AVAILABLE + execution.status() + "...");
        }
        ReportFile file = launcher.report(execution)
                .orElseThrow(() -> new RecordNotFoundException(MSG_REPORT_NOT_RETAINED));
        return ResponseEntity.ok()
                .header(HttpHeaders.CONTENT_DISPOSITION,
                        ContentDisposition.attachment().filename(file.fileName()).build().toString())
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .body(file.content());
    }

    @ExceptionHandler(ReportQueueFullException.class)
    ProblemDetail queueFull(ReportQueueFullException e) {
        ProblemDetail problem = ApiErrors.problem(HttpStatus.SERVICE_UNAVAILABLE, "NOSPACE", null, e.getMessage());
        problem.setProperty("cicsResp", "NOSPACE");
        problem.setProperty("executionId", e.executionId());
        return problem;
    }

    private ReportExecution visible(Jwt jwt, long executionId) {
        CurrentUser user = CurrentUser.of(jwt);
        return launcher.find(executionId)
                .filter(e -> user.userType() == UserType.ADMIN || e.requestedBy().equals(user.userId()))
                .orElseThrow(() -> new RecordNotFoundException(MSG_EXECUTION_NOT_FOUND));
    }

    private static ReportExecutionResponse.JobRun jobRun(BatchRun run) {
        return new ReportExecutionResponse.JobRun(run.jobExecutionId(), run.jobName(), run.status(),
                run.returnCode() == null ? null : run.returnCode().label(), run.readCount(), run.writeCount(),
                run.startTime(), run.endTime());
    }

    private static ReportExecutionResponse.Report report(ReportExecution execution, ReportFile file) {
        return new ReportExecutionResponse.Report(file.fileName(), file.catalog().getOutputFileId(),
                file.catalog().getRecordCount(), file.catalog().getSha256(), execution.encoding(),
                PATH + "/" + execution.executionId() + "/report", file.lines());
    }

    static NavigationContext exit() {
        return NavigationContext.transfer(TRAN_ID, PROGRAM, "CM00", "COMEN01C");
    }
}
