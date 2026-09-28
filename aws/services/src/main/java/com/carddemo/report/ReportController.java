package com.carddemo.report;

import com.carddemo.report.ReportService.StatusResponse;
import com.carddemo.report.ReportService.SubmitRequest;
import com.carddemo.report.ReportService.SubmitResponse;
import com.carddemo.security.CurrentUser;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/reports/transactions")
public class ReportController {

    private final ReportService service;

    public ReportController(ReportService service) {
        this.service = service;
    }

    @PostMapping
    public ResponseEntity<SubmitResponse> submit(@RequestBody(required = false) SubmitRequest request) {
        return ResponseEntity.status(HttpStatus.ACCEPTED).body(service.submit(request, CurrentUser.userId()));
    }

    @GetMapping("/{requestId}")
    public StatusResponse status(@PathVariable String requestId) {
        return service.status(requestId);
    }
}
