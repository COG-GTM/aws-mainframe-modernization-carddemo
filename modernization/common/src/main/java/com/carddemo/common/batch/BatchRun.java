package com.carddemo.common.batch;

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.StepExecution;

import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/** One finished launch: the job execution, its output generation and the step condition codes. */
public record BatchRun(String job, JobExecution execution, Path outputDir, int returnCode) {

    /** JSON-friendly summary for the HTTP response and the CLI log line. */
    public Map<String, Object> summary() {
        Map<String, Object> results = new LinkedHashMap<>();
        List<Map<String, Object>> steps = execution.getStepExecutions().stream().map(step -> {
            Map<String, Object> s = new LinkedHashMap<>();
            s.put("step", step.getStepName());
            s.put("status", step.getStatus().name());
            s.put("exitCode", step.getExitStatus().getExitCode());
            Integer rc = ReturnCodes.of(step);
            if (rc != null) {
                s.put("returnCode", rc);
            }
            step.getExecutionContext().entrySet().stream()
                    .filter(e -> !e.getKey().startsWith("jcl.") && !e.getKey().startsWith("batch."))
                    .forEach(e -> results.put(e.getKey(), e.getValue()));
            if (ReturnCodes.bypassed(step)) {
                s.put("bypassed", step.getExecutionContext().getString(ReturnCodes.BYPASSED));
            }
            failure(step).ifPresent(f -> s.put("failure", f));
            return s;
        }).toList();
        Map<String, Object> summary = new LinkedHashMap<>();
        summary.put("job", job);
        summary.put("jobName", execution.getJobInstance().getJobName());
        summary.put("executionId", execution.getId());
        summary.put("status", execution.getStatus().name());
        summary.put("returnCode", returnCode);
        summary.put("outputDir", outputDir.toAbsolutePath().toString());
        summary.put("results", results);
        summary.put("steps", steps);
        return summary;
    }

    private static java.util.Optional<String> failure(StepExecution step) {
        return step.getFailureExceptions().stream().findFirst()
                .map(ex -> ex.getClass().getSimpleName() + ": " + ex.getMessage());
    }
}
