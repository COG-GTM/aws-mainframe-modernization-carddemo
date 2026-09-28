package com.carddemo.batch.core;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.sql.Timestamp;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

/** {@code batch_job_run(run_id, job_name, business_date, status, exit_code, started_at, ended_at, counts)}. */
@Repository
public class BatchRunRepository {

    public static final String RUNNING = "RUNNING";
    public static final String COMPLETED = "COMPLETED";
    public static final String FAILED = "FAILED";
    /** AWS Batch {@code timeout.attemptDurationSeconds} of the job definitions. */
    static final Duration STALE_RUNNING = Duration.ofHours(1);

    private final JdbcTemplate jdbc;
    private final ObjectMapper mapper;

    public BatchRunRepository(JdbcTemplate jdbc, ObjectMapper mapper) {
        this.jdbc = jdbc;
        this.mapper = mapper;
    }

    public record Run(String status, Integer exitCode, String counts) {
    }

    public Optional<Run> find(String runId, String jobName) {
        List<Run> runs = jdbc.query(
                "SELECT status, exit_code, counts::text AS counts FROM batch_job_run WHERE run_id = ? AND job_name = ?",
                (rs, i) -> new Run(rs.getString("status"), (Integer) rs.getObject("exit_code"), rs.getString("counts")),
                runId, jobName);
        return runs.stream().findFirst();
    }

    /**
     * Atomically claims {@code (runId, jobName)}: inserts a RUNNING row, or takes over a FAILED one or a RUNNING
     * one older than the Batch attempt timeout (its container is gone). Returns false when another process holds
     * the claim or the run already completed.
     */
    public boolean claim(String runId, String jobName, LocalDate businessDate) {
        Instant now = Instant.now();
        return jdbc.update("""
                INSERT INTO batch_job_run (run_id, job_name, business_date, status, started_at)
                VALUES (?, ?, ?, 'RUNNING', ?)
                ON CONFLICT (run_id, job_name) DO UPDATE
                   SET status = 'RUNNING', exit_code = NULL, started_at = EXCLUDED.started_at, ended_at = NULL
                 WHERE batch_job_run.status = 'FAILED'
                    OR (batch_job_run.status = 'RUNNING' AND batch_job_run.started_at < ?)
                """, runId, jobName, businessDate, Timestamp.from(now),
                Timestamp.from(now.minus(STALE_RUNNING))) == 1;
    }

    public void finish(String runId, String jobName, int returnCode, Map<String, Object> counts) {
        jdbc.update("""
                UPDATE batch_job_run SET status = ?, exit_code = ?, ended_at = ?,
                       counts = COALESCE(counts, '{}'::jsonb) || ?::jsonb
                 WHERE run_id = ? AND job_name = ?
                """, returnCode <= ReturnCode.WARNING ? COMPLETED : FAILED, returnCode,
                Timestamp.from(Instant.now()), toJson(counts), runId, jobName);
    }

    private String toJson(Map<String, Object> counts) {
        try {
            return mapper.writeValueAsString(counts);
        } catch (JsonProcessingException e) {
            throw new IllegalStateException(e);
        }
    }
}
