package com.carddemo.batch.harness;

import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.springframework.batch.core.JobParameter;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;

/**
 * A multi-step JCL job: a {@link JobChain} of harness jobs launched by name from the CLI ({@code --job=<name>}). Each
 * step gets the CLI parameters; {@code --STEPnn.<name>=<value>} applies to step {@code STEPnn} only (e.g.
 * {@code --STEP10.SYSOUT=...}). The CLI exit code is the chain's highest RC; every launched step has its own
 * {@code batch_run} rows.
 */
public interface JobStream {

    String name();

    JobChain chain(BatchJobLauncher launcher, JobParameters parameters);

    /** The parameters of step {@code stepName}: unqualified ones, overridden by {@code <stepName>.<name>}. */
    static JobParameters forStep(JobParameters parameters, String stepName) {
        Pattern qualified = Pattern.compile("(STEP\\w*)\\.(.+)");
        JobParametersBuilder builder = new JobParametersBuilder();
        Map<String, JobParameter<?>> all = parameters.getParameters();
        all.forEach((name, value) -> {
            if (!qualified.matcher(name).matches()) {
                builder.addJobParameter(name, value);
            }
        });
        all.forEach((name, value) -> {
            Matcher m = qualified.matcher(name);
            if (m.matches() && m.group(1).equals(stepName)) {
                builder.addJobParameter(m.group(2), value);
            }
        });
        return builder.toJobParameters();
    }
}
