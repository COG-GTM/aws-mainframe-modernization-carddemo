package com.carddemo.modernization.batch;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.regex.Pattern;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobExecutionListener;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersInvalidException;
import org.springframework.batch.core.JobParametersValidator;

/**
 * GDG {@code (+1)} outputs become files in {@code <run-root>/<runId>/}. The directory is created when the job starts
 * and must not exist yet, so a run never overwrites an earlier generation.
 */
final class RunDirectory implements JobExecutionListener, JobParametersValidator {
    static final String RUN_ID = "runId";
    static final String PARM_DATE = "parmDate";
    private static final Pattern SIMPLE_NAME = Pattern.compile("[A-Za-z0-9][A-Za-z0-9._-]*");

    private final Path runRoot;

    RunDirectory(Path runRoot) {
        this.runRoot = runRoot;
    }

    static Path of(Path runRoot, JobParameters parameters) {
        return runRoot.resolve(parameters.getString(RUN_ID));
    }

    @Override
    public void validate(JobParameters parameters) throws JobParametersInvalidException {
        String runId = parameters == null ? null : parameters.getString(RUN_ID);
        if (runId == null || !SIMPLE_NAME.matcher(runId).matches()) {
            throw new JobParametersInvalidException("runId must be a simple directory name, got " + runId);
        }
        String parmDate = parameters.getString(PARM_DATE);
        if (parmDate == null || parmDate.length() != 10) {
            throw new JobParametersInvalidException("parmDate (INTCALC PARM) must be exactly 10 characters, got "
                    + parmDate);
        }
    }

    @Override
    public void beforeJob(JobExecution jobExecution) {
        Path dir = of(runRoot, jobExecution.getJobParameters());
        try {
            Files.createDirectories(runRoot);
            Files.createDirectory(dir);
        } catch (IOException e) {
            throw new UncheckedIOException("run directory " + dir + " must not exist yet", e);
        }
    }
}
