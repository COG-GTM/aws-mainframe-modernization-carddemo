package com.carddemo.batch;

import com.carddemo.common.error.ApiException;
import com.carddemo.migration.SourceEncoding;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.time.LocalDate;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

/** Operator API replacing JES job submission (SUBMIT / CA7-style scheduling) for the CardDemo batch suite. */
@RestController
@RequestMapping("/api/v1/batch")
@PreAuthorize("hasRole('ADMIN')")
public class BatchController {

    private final JobLauncher launcher;
    private final Map<String, Job> jobs;
    private final Map<Long, JobResult> results;
    private final DailyTransactionStager stager;
    private final TransactionReportService reports;

    public BatchController(JobLauncher launcher, List<Job> jobs, Map<Long, JobResult> jobResults,
                           DailyTransactionStager stager, TransactionReportService reports) {
        this.launcher = launcher;
        this.jobs = new LinkedHashMap<>();
        jobs.forEach(j -> this.jobs.put(j.getName(), j));
        this.results = jobResults;
        this.stager = stager;
        this.reports = reports;
    }

    @GetMapping("/jobs")
    public List<String> jobs() {
        return List.copyOf(jobs.keySet());
    }

    /** POST /jobs/INTCALC?date=2024-01-31, /jobs/TRANREPT?startDate=..&endDate=.., /jobs/POSTTRAN, /jobs/CREASTMT. */
    @PostMapping("/jobs/{name}")
    public Map<String, Object> run(@PathVariable String name, @RequestParam Map<String, String> params)
            throws Exception {
        Job job = jobs.get(name.toUpperCase());
        if (job == null) {
            throw ApiException.notFound("JOB_NOT_FOUND", "Unknown job " + name);
        }
        JobParametersBuilder b = new JobParametersBuilder().addLong("run.ts", System.nanoTime());
        if (job.getName().equals("INTCALC")) {
            b.addString("date", params.getOrDefault("date", LocalDate.now().toString()));
        }
        if (job.getName().equals("TRANREPT")) {
            b.addString("startDate", required(params, "startDate"));
            b.addString("endDate", required(params, "endDate"));
        }
        JobExecution exec = launcher.run(job, b.toJobParameters());
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("jobExecutionId", exec.getId());
        body.put("job", job.getName());
        body.put("status", exec.getStatus().toString());
        JobResult r = results.get(exec.getId());
        if (r != null) {
            body.put("returnCode", r.returnCode());
            body.put("counters", r.counters());
            body.put("output", r.output());
        } else {
            body.put("failures", exec.getAllFailureExceptions().stream().map(Throwable::getMessage).toList());
        }
        return body;
    }

    /** DALYTRAN drop: raw fixed-width file body (text/plain for ASCII, application/octet-stream for EBCDIC). */
    @PostMapping(value = "/daily-transactions", consumes = {"text/plain", "application/octet-stream"})
    @ResponseStatus(HttpStatus.ACCEPTED)
    public Map<String, Object> stage(@RequestBody byte[] file,
                                     @RequestParam(defaultValue = "ASCII") SourceEncoding encoding)
            throws IOException {
        return Map.of("staged", stager.stage(new ByteArrayInputStream(file), encoding));
    }

    @PostMapping("/report-requests/process")
    public Map<String, Object> processReports() {
        return Map.of("processed", reports.processSubmittedRequests());
    }

    private static String required(Map<String, String> p, String key) {
        String v = p.get(key);
        if (v == null || v.isBlank()) {
            throw ApiException.badRequest("PARAM_REQUIRED", key + " is required");
        }
        return v;
    }
}
