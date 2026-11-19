package com.carddemo.transaction.batch;

import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersInvalidException;
import org.springframework.batch.core.JobParametersValidator;
import org.springframework.batch.core.explore.JobExplorer;

import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Launch checks that make the run a JCL error (exit 12, nothing processed) rather than an abend: DALYTRAN must be bound,
 * and a new instance must not find an existing DALYREJS, because the GDG {@code (+1)} is always {@code DISP=NEW}
 * ({@code POSTTRAN.jcl:34-39}; FR-POSTTRAN-17, D-016). A restart reuses its own DALYREJS (D-012).
 */
public class PosttranParametersValidator implements JobParametersValidator {

    private final JobExplorer jobExplorer;
    private final Path outputDir;

    public PosttranParametersValidator(JobExplorer jobExplorer, Path outputDir) {
        this.jobExplorer = jobExplorer;
        this.outputDir = outputDir;
    }

    @Override
    public void validate(JobParameters parameters) throws JobParametersInvalidException {
        if (parameters == null || parameters.getString(PosttranDatasets.DALYTRAN) == null) {
            throw new JobParametersInvalidException("POSTTRAN needs --dd=DALYTRAN=<path>");
        }
        boolean restart = jobExplorer.getJobInstance(PosttranJobConfig.JOB, parameters) != null;
        Path dalyrejs = PosttranDatasets.dalyrejs(parameters, outputDir);
        if (!restart && Files.exists(dalyrejs)) {
            throw new JobParametersInvalidException("DALYREJS " + dalyrejs + " already exists; a new generation is never "
                    + "overwritten (DISP=NEW)");
        }
    }
}
