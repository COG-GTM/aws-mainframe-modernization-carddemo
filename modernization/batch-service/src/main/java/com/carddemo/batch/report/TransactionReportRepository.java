package com.carddemo.batch.report;

import java.nio.file.Path;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.Optional;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

/**
 * {@code transaction_report_request} in the Flyway default schema ({@code batch}), through JDBC like the
 * Spring Batch tables beside it.
 */
@Repository
public class TransactionReportRepository {

    private final JdbcTemplate jdbc;
    private final String table;

    public TransactionReportRepository(JdbcTemplate jdbc,
                                       @Value("${spring.flyway.default-schema:}") String schema) {
        this.jdbc = jdbc;
        this.table = (schema.isBlank() ? "" : schema + ".") + "transaction_report_request";
    }

    public void insert(TransactionReport r) {
        jdbc.update("""
                INSERT INTO %s
                    (report_id, report_type, start_date, end_date, status, submitted_by, submitted_at)
                VALUES (?, ?, ?, ?, ?, ?, ?)""".formatted(table),
                r.reportId(), r.reportType().name(), r.startDate(), r.endDate(), r.status().name(),
                r.submittedBy(), Timestamp.from(r.submittedAt()));
    }

    public void markRunning(String reportId, Instant at) {
        jdbc.update("UPDATE " + table + " SET status = 'RUNNING', started_at = ? WHERE report_id = ?",
                Timestamp.from(at), reportId);
    }

    public void markFinished(String reportId, TransactionReport.Status status, Instant at, Long jobExecutionId,
                             Integer returnCode, Path outputDir, String message) {
        jdbc.update("""
                UPDATE %s
                   SET status = ?, completed_at = ?, job_execution_id = ?, return_code = ?, output_dir = ?, message = ?
                 WHERE report_id = ?""".formatted(table),
                status.name(), Timestamp.from(at), jobExecutionId, returnCode,
                outputDir == null ? null : outputDir.toString(), message, reportId);
    }

    /** Requests a previous batch-service process accepted but never finished. */
    public int failUnfinished(Instant at, String message) {
        return jdbc.update("""
                UPDATE %s
                   SET status = 'FAILED', completed_at = ?, message = ?
                 WHERE status IN ('SUBMITTED', 'RUNNING')""".formatted(table), Timestamp.from(at), message);
    }

    public Optional<TransactionReport> find(String reportId) {
        return jdbc.query("SELECT * FROM " + table + " WHERE report_id = ?",
                TransactionReportRepository::map, reportId).stream().findFirst();
    }

    private static TransactionReport map(ResultSet rs, int row) throws SQLException {
        String outputDir = rs.getString("output_dir");
        return new TransactionReport(rs.getString("report_id"),
                ReportType.valueOf(rs.getString("report_type")),
                rs.getDate("start_date").toLocalDate(), rs.getDate("end_date").toLocalDate(),
                TransactionReport.Status.valueOf(rs.getString("status")),
                rs.getString("submitted_by"), instant(rs, "submitted_at"), instant(rs, "started_at"),
                instant(rs, "completed_at"), (Long) rs.getObject("job_execution_id", Long.class),
                (Integer) rs.getObject("return_code", Integer.class),
                outputDir == null ? null : Path.of(outputDir), rs.getString("message"));
    }

    private static Instant instant(ResultSet rs, String column) throws SQLException {
        Timestamp ts = rs.getTimestamp(column);
        return ts == null ? null : ts.toInstant();
    }
}
