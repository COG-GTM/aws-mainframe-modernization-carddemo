package com.carddemo.batch.report;

import com.carddemo.common.error.NotFoundException;
import com.carddemo.common.security.CallerIdentity;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * CICS CR00 / CORPT00C, main menu option 9 (any signed-on user). POST validates as the screen did and
 * answers 202 with the report id ({@code Location} points at it); GET polls; {@code /content} returns
 * the TRANREPT file once the job completed.
 */
@RestController
@RequestMapping("/api/v1/reports/transactions")
public class TransactionReportController {

    static final String NOT_READY = "Report is not available yet...";

    private final TransactionReportService service;

    public TransactionReportController(TransactionReportService service) {
        this.service = service;
    }

    @PostMapping
    public ResponseEntity<TransactionReportResponse> submit(@Valid @RequestBody TransactionReportRequest request,
                                                            HttpServletRequest http) {
        TransactionReport report = service.submit(request, caller(http), http.getHeader(HttpHeaders.AUTHORIZATION));
        return ResponseEntity.accepted()
                .location(URI.create("/api/v1/reports/transactions/" + report.reportId()))
                .body(TransactionReportResponse.from(report));
    }

    @GetMapping("/{reportId}")
    public TransactionReportResponse get(@PathVariable String reportId, HttpServletRequest http) {
        return TransactionReportResponse.from(service.get(reportId, caller(http)));
    }

    @GetMapping("/{reportId}/content")
    public ResponseEntity<Resource> content(@PathVariable String reportId, HttpServletRequest http) throws IOException {
        TransactionReport report = service.get(reportId, caller(http));
        if (report.status() != TransactionReport.Status.COMPLETED || !Files.isRegularFile(report.reportFile())) {
            throw new NotFoundException(NOT_READY);
        }
        return ResponseEntity.ok()
                .contentType(new MediaType("text", "plain", java.nio.charset.StandardCharsets.ISO_8859_1))
                .header(HttpHeaders.CONTENT_DISPOSITION, "inline; filename=\"TRANREPT-" + reportId + ".txt\"")
                .body(new FileSystemResource(report.reportFile()));
    }

    private static CallerIdentity caller(HttpServletRequest http) {
        // The bearer-token filter guarantees one on /api/**.
        return CallerIdentity.from(http).orElseThrow();
    }
}
