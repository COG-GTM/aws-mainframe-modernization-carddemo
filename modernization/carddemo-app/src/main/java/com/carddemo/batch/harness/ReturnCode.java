package com.carddemo.batch.harness;

import com.carddemo.common.AbendException;
import com.carddemo.common.codec.RecordFormatException;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.StepExecution;

/**
 * JCL step condition codes (ADR-0015): what a step's {@code RETURN-CODE} or abend becomes in the job log, the
 * {@code batch_run} table and the process exit code of the batch CLI.
 *
 * <ul>
 *   <li>A step that completes has RC 0 unless it raised it with {@link #set}.</li>
 *   <li>A step that fails: an abend ({@link AbendException}: {@code CEE3ABD}, or an I/O error the program did not
 *       handle, {@code FileStatusException}) or a launch that never ran = 16; a {@link ReturnCodeException}
 *       carries its own code; invalid data ({@code RecordFormatException}) = 8; anything else unexpected = 12.</li>
 *   <li>A job's RC is the highest RC of its steps (JCL MAXCC); a job that did not complete is at least 8.</li>
 * </ul>
 */
public enum ReturnCode {
    OK(0),
    WARNING(4),
    ERROR(8),
    SEVERE(12),
    TERMINAL(16);

    /** Step execution-context key holding the RC a step set explicitly. */
    public static final String CONTEXT_KEY = "carddemo.return-code";

    private final int code;

    ReturnCode(int code) {
        this.code = code;
    }

    public int code() {
        return code;
    }

    /** {@code RC=0004}, as a job log prints it. */
    public String label() {
        return String.format(java.util.Locale.ROOT, "RC=%04d", code);
    }

    public boolean isFailure() {
        return code >= ERROR.code;
    }

    public ReturnCode max(ReturnCode other) {
        return other != null && other.code > code ? other : this;
    }

    public static ReturnCode of(int code) {
        return Arrays.stream(values()).filter(rc -> rc.code == code).findFirst()
                .orElseThrow(() -> new IllegalArgumentException("return codes are 0, 4, 8, 12, 16; got " + code));
    }

    /** {@code MOVE n TO RETURN-CODE}: raises (never lowers) the RC of the running step. */
    public static void set(StepExecution step, ReturnCode rc) {
        ReturnCode current = explicit(step).orElse(OK);
        step.getExecutionContext().putInt(CONTEXT_KEY, current.max(rc).code);
    }

    public static ReturnCode of(StepExecution step) {
        ReturnCode explicit = explicit(step).orElse(OK);
        BatchStatus status = step.getStatus();
        if (status == BatchStatus.COMPLETED) {
            return explicit;
        }
        if (status == BatchStatus.FAILED) {
            return explicit.max(classify(step.getFailureExceptions()));
        }
        return explicit.max(status.isRunning() ? OK : TERMINAL);
    }

    public static ReturnCode of(JobExecution job) {
        ReturnCode rc = OK;
        for (StepExecution step : job.getStepExecutions()) {
            rc = rc.max(of(step));
        }
        BatchStatus status = job.getStatus();
        if (status == BatchStatus.COMPLETED || status.isRunning()) {
            return rc;
        }
        if (status == BatchStatus.FAILED) {
            rc = rc.max(classify(job.getFailureExceptions()));
            return rc.max(ERROR);
        }
        return rc.max(TERMINAL);
    }

    /** The RC of a failure, from the most specific exception found in each cause chain. */
    public static ReturnCode classify(List<Throwable> failures) {
        ReturnCode rc = OK;
        for (Throwable failure : failures) {
            rc = rc.max(classify(failure));
        }
        return rc;
    }

    public static ReturnCode classify(Throwable failure) {
        if (findCause(failure, AbendException.class).isPresent()) {
            return TERMINAL;
        }
        Optional<ReturnCodeException> explicit = findCause(failure, ReturnCodeException.class);
        if (explicit.isPresent()) {
            return explicit.get().returnCode();
        }
        if (findCause(failure, RecordFormatException.class).isPresent()) {
            return ERROR;
        }
        return SEVERE;
    }

    public static boolean isAbend(List<Throwable> failures) {
        return failures.stream().anyMatch(f -> findCause(f, AbendException.class).isPresent());
    }

    static <T extends Throwable> Optional<T> findCause(Throwable failure, Class<T> type) {
        for (Throwable t = failure; t != null; t = t.getCause() == t ? null : t.getCause()) {
            if (type.isInstance(t)) {
                return Optional.of(type.cast(t));
            }
        }
        return Optional.empty();
    }

    private static Optional<ReturnCode> explicit(StepExecution step) {
        return step.getExecutionContext().containsKey(CONTEXT_KEY)
                ? Optional.of(of(step.getExecutionContext().getInt(CONTEXT_KEY)))
                : Optional.empty();
    }
}
