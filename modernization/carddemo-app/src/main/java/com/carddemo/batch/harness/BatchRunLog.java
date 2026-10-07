package com.carddemo.batch.harness;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParameter;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.StepExecution;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

/** Writes and reads {@code batch_run} (Flyway V4, ADR-0015). */
@Component
public class BatchRunLog {

    static final int TEXT_LIMIT = 2500;

    private static final String INSERT = """
            insert into batch_run (job_execution_id, step_execution_id, job_name, step_name, run_date, status,
                                   exit_code, return_code, read_count, write_count, skip_count, filter_count,
                                   start_time, end_time, parameters, message)
            values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """;

    private final JdbcTemplate jdbc;

    public BatchRunLog(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    /** The job row plus one row per step of a finished job execution; returns the job's RC. */
    public ReturnCode record(JobExecution job) {
        ReturnCode jobRc = ReturnCode.of(job);
        LocalDate runDate = runDate(job.getJobParameters());
        String parameters = describe(job.getJobParameters());
        String jobName = job.getJobInstance().getJobName();
        long read = 0;
        long write = 0;
        long skip = 0;
        long filter = 0;
        for (StepExecution step : job.getStepExecutions()) {
            read += step.getReadCount();
            write += step.getWriteCount();
            skip += step.getSkipCount();
            filter += step.getFilterCount();
        }
        jdbc.update(INSERT, job.getId(), null, jobName, null, runDate, job.getStatus().name(),
                clip(job.getExitStatus().getExitCode()), jobRc.code(), read, write, skip, filter, job.getStartTime(),
                job.getEndTime(), parameters, clip(failureMessage(job.getAllFailureExceptions())));
        for (StepExecution step : job.getStepExecutions()) {
            jdbc.update(INSERT, job.getId(), step.getId(), jobName, step.getStepName(), runDate,
                    step.getStatus().name(), clip(step.getExitStatus().getExitCode()), ReturnCode.of(step).code(),
                    step.getReadCount(), step.getWriteCount(), step.getSkipCount(), step.getFilterCount(),
                    step.getStartTime(), step.getEndTime(), null, clip(failureMessage(step.getFailureExceptions())));
        }
        return jobRc;
    }

    /** A launch that never produced a job execution (JCL error): RC 16. */
    public void recordLaunchFailure(String jobName, JobParameters parameters, String message) {
        jdbc.update(INSERT, null, null, clip100(jobName == null ? "?" : jobName), null,
                parameters == null ? null : runDate(parameters), "ABANDONED", "NOT_LAUNCHED",
                ReturnCode.TERMINAL.code(), 0, 0, 0, 0, null, null,
                parameters == null ? null : describe(parameters), clip(message));
    }

    /** Job row first, then its steps in execution order. */
    public List<BatchRun> findByJobExecutionId(long jobExecutionId) {
        return jdbc.query("select * from batch_run where job_execution_id = ?"
                + " order by step_name is not null, step_execution_id, batch_run_id", BatchRunLog::map,
                jobExecutionId);
    }

    public List<BatchRun> findByJobName(String jobName) {
        return jdbc.query("select * from batch_run where job_name = ? order by batch_run_id", BatchRunLog::map,
                jobName);
    }

    static LocalDate runDate(JobParameters parameters) {
        JobParameter<?> p = parameters.getParameters().get(BatchCommandLine.RUN_DATE);
        if (p == null) {
            return null;
        }
        Object value = p.getValue();
        return value instanceof LocalDate d ? d : LocalDate.parse(value.toString());
    }

    private static String describe(JobParameters parameters) {
        return clip(parameters.getParameters().entrySet().stream()
                .map(e -> e.getKey() + "=" + e.getValue().getValue())
                .collect(Collectors.joining(",")));
    }

    private static String failureMessage(List<Throwable> failures) {
        return failures.isEmpty() ? null : failures.stream().map(t -> t.getClass().getSimpleName() + ": "
                + t.getMessage()).collect(Collectors.joining("; "));
    }

    private static String clip(String text) {
        return text == null || text.length() <= TEXT_LIMIT ? text : text.substring(0, TEXT_LIMIT);
    }

    private static String clip100(String text) {
        return text.length() <= 100 ? text : text.substring(0, 100);
    }

    private static BatchRun map(ResultSet rs, int row) throws SQLException {
        return new BatchRun(rs.getLong("batch_run_id"), rs.getObject("job_execution_id", Long.class),
                rs.getObject("step_execution_id", Long.class), rs.getString("job_name"), rs.getString("step_name"),
                rs.getObject("run_date", LocalDate.class), rs.getString("status"), rs.getString("exit_code"),
                ReturnCode.of(rs.getInt("return_code")), rs.getLong("read_count"), rs.getLong("write_count"),
                rs.getLong("skip_count"), rs.getLong("filter_count"),
                rs.getObject("start_time", java.time.LocalDateTime.class),
                rs.getObject("end_time", java.time.LocalDateTime.class), rs.getString("parameters"),
                rs.getString("message"));
    }
}
