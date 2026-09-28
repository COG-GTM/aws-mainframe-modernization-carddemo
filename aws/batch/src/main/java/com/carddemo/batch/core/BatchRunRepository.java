package com.carddemo.batch.core;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

/** {@code batch_job_run(run_id, job_name, business_date, status, exit_code, started_at, ended_at, counts)}. */
@Repository
public class BatchRunRepository {

    public static final String RUNNING = "RUNNING";
    public static final String COMPLETED = "COMPLETED";
    public static final String FAILED = "FAILED";

    private final JdbcTemplate jdbc;
    private final ObjectMapper mapper;

    public BatchRunRepository(JdbcTemplate jdbc, ObjectMapper mapper) {
        this.jdbc = jdbc;
        this.mapper = mapper;
    }

    /** {@code params} are the job-specific parameters the run was started with ({@code null} if not recorded). */
    public record Run(String status, Integer exitCode, String counts, LocalDate businessDate, String params) {
    }

    public Optional<Run> find(String runId, String jobName) {
        List<Run> runs = jdbc.query(
                "SELECT status, exit_code, (counts - 'params')::text AS counts, business_date,"
                        + " (counts->'params')::text AS params FROM batch_job_run"
                        + " WHERE run_id = ? AND job_name = ?",
                (rs, i) -> new Run(rs.getString("status"), (Integer) rs.getObject("exit_code"), rs.getString("counts"),
                        rs.getObject("business_date", LocalDate.class), rs.getString("params")),
                runId, jobName);
        return runs.stream().findFirst();
    }

    /**
     * Marks {@code (runId, jobName)} RUNNING unless it already completed. Callers hold the run's
     * {@link AdvisoryLock}, so a leftover RUNNING row belongs to a dead attempt and is taken over.
     */
    public boolean start(String runId, String jobName, LocalDate businessDate, Map<String, String> params) {
        String p = paramsJson(params);
        return jdbc.update("""
                INSERT INTO batch_job_run (run_id, job_name, business_date, status, started_at, counts)
                VALUES (?, ?, ?, 'RUNNING', ?, jsonb_build_object('params', ?::jsonb))
                ON CONFLICT (run_id, job_name) DO UPDATE
                   SET status = 'RUNNING', exit_code = NULL, started_at = EXCLUDED.started_at, ended_at = NULL,
                       counts = COALESCE(batch_job_run.counts, '{}'::jsonb) || EXCLUDED.counts
                 WHERE batch_job_run.status <> 'COMPLETED'
                """, runId, jobName, businessDate, Timestamp.from(Instant.now()), p) == 1;
    }

    /** Canonical (key-sorted) JSON of job-specific parameters, comparable with {@link Run#params()}. */
    public String paramsJson(Map<String, String> params) {
        try {
            return mapper.writeValueAsString(new TreeMap<>(params));
        } catch (JsonProcessingException e) {
            throw new IllegalStateException(e);
        }
    }

    public boolean sameParams(String recorded, Map<String, String> params) {
        try {
            return mapper.readTree(recorded).equals(mapper.readTree(paramsJson(params)));
        } catch (JsonProcessingException e) {
            return false;
        }
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
