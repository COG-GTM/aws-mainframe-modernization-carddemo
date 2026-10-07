package com.carddemo.batch.report;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.support.GeneratedKeyHolder;
import org.springframework.jdbc.support.KeyHolder;
import org.springframework.stereotype.Repository;

/** {@code report_request} rows (Flyway V5). Each update auto-commits so pollers see progress immediately. */
@Repository
public class ReportRequestStore {

    private final JdbcTemplate jdbc;
    private final NamedParameterJdbcTemplate named;

    public ReportRequestStore(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
        this.named = new NamedParameterJdbcTemplate(jdbc);
    }

    long insert(String jobStream, ReportWindow window, LocalDate runDate, String encoding, String requestedBy) {
        KeyHolder key = new GeneratedKeyHolder();
        named.update("""
                insert into report_request (job_stream, report_name, start_date, end_date, run_date, encoding,
                    requested_by, status)
                values (:stream, :name, :start, :end, :runDate, :encoding, :requestedBy, 'QUEUED')""",
                new MapSqlParameterSource()
                        .addValue("stream", jobStream)
                        .addValue("name", window.name().label())
                        .addValue("start", window.startDate())
                        .addValue("end", window.endDate())
                        .addValue("runDate", runDate)
                        .addValue("encoding", encoding)
                        .addValue("requestedBy", requestedBy),
                key, new String[] {"report_request_id"});
        return key.getKey().longValue();
    }

    void running(long id, OffsetDateTime at) {
        jdbc.update("update report_request set status = 'RUNNING', started_at = ? where report_request_id = ?",
                at, id);
    }

    void finished(long id, ReportStatus status, Integer returnCode, List<Long> jobExecutionIds,
                  Long reportJobExecutionId, Long outputFileId, String message, OffsetDateTime at) {
        named.update("""
                update report_request set status = :status, return_code = :rc, job_execution_ids = :ids,
                    report_job_execution_id = :reportJob, output_file_id = :file, message = :message, ended_at = :at
                where report_request_id = :id""",
                new MapSqlParameterSource()
                        .addValue("status", status.name())
                        .addValue("rc", returnCode, Types.SMALLINT)
                        .addValue("ids", jobExecutionIds.isEmpty() ? null : jobExecutionIds.stream()
                                .map(String::valueOf).collect(Collectors.joining(",")))
                        .addValue("reportJob", reportJobExecutionId, Types.BIGINT)
                        .addValue("file", outputFileId, Types.BIGINT)
                        .addValue("message", truncate(message))
                        .addValue("at", at)
                        .addValue("id", id));
    }

    public Optional<ReportExecution> find(long id) {
        return jdbc.query("select * from report_request where report_request_id = ?", ReportRequestStore::row, id)
                .stream().findFirst();
    }

    private static ReportExecution row(ResultSet rs, int rowNum) throws SQLException {
        String ids = rs.getString("job_execution_ids");
        int rc = rs.getInt("return_code");
        Integer returnCode = rs.wasNull() ? null : rc;
        return new ReportExecution(rs.getLong("report_request_id"), rs.getString("job_stream"),
                ReportName.ofLabel(rs.getString("report_name")), rs.getObject("start_date", LocalDate.class),
                rs.getObject("end_date", LocalDate.class), rs.getObject("run_date", LocalDate.class),
                rs.getString("encoding"), rs.getString("requested_by"), ReportStatus.valueOf(rs.getString("status")),
                returnCode, ids == null ? List.of() : Arrays.stream(ids.split(",")).map(Long::valueOf).toList(),
                (Long) rs.getObject("report_job_execution_id"), (Long) rs.getObject("output_file_id"),
                rs.getString("message"), rs.getObject("submitted_at", OffsetDateTime.class),
                rs.getObject("started_at", OffsetDateTime.class), rs.getObject("ended_at", OffsetDateTime.class));
    }

    private static String truncate(String message) {
        return message == null || message.length() <= 2500 ? message : message.substring(0, 2500);
    }
}
