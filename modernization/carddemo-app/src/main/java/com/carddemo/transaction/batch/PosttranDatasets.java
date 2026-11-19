package com.carddemo.transaction.batch;

import com.carddemo.common.batch.JclJobLauncher;
import org.springframework.batch.core.JobParameters;

import java.nio.file.Path;

/**
 * Dataset resolution of {@code POSTTRAN.jcl} STEP15 (D-014, D-016): {@code --dd=DALYTRAN} is required;
 * {@code --dd=DALYREJS} defaults to {@code <carddemo.batch.output-dir>/POSTTRAN/<run.id>/DALYREJS} (a new GDG
 * generation per run); {@code --dd=SYSOUT} is optional.
 */
public final class PosttranDatasets {

    public static final String DALYTRAN = JclJobLauncher.DD_PREFIX + "DALYTRAN";
    public static final String DALYREJS = JclJobLauncher.DD_PREFIX + "DALYREJS";
    public static final String SYSOUT = JclJobLauncher.DD_PREFIX + "SYSOUT";

    private PosttranDatasets() {
    }

    public static Path dalyrejs(JobParameters parameters, Path outputDir) {
        String bound = parameters.getString(DALYREJS);
        if (bound != null) {
            return Path.of(bound);
        }
        return outputDir.resolve(PosttranJobConfig.JOB).resolve(parameters.getString(JclJobLauncher.RUN_ID)).resolve("DALYREJS");
    }
}
