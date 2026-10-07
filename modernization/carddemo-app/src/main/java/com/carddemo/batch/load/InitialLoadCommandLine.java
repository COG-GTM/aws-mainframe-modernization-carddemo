package com.carddemo.batch.load;

import com.carddemo.batch.harness.CommandLineJobParameters;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Locale;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.stereotype.Component;

/**
 * {@code --job=initial-load [--source-dir=<dir>] [--mode=REPLACE|UPSERT]}: defaults from
 * {@code carddemo.initial-load.*}, plus the {@code source-sha256} identity the job expects.
 */
@Component
class InitialLoadCommandLine implements CommandLineJobParameters {

    private final InitialLoadProperties properties;

    InitialLoadCommandLine(InitialLoadProperties properties) {
        this.properties = properties;
    }

    @Override
    public String jobName() {
        return InitialLoadJobConfiguration.JOB_NAME;
    }

    @Override
    public JobParameters adapt(JobParameters raw) {
        String dir = raw.getString(InitialLoadJobConfiguration.SOURCE_DIR);
        Path sourceDir = dir == null ? properties.resolvedSourceDir() : Path.of(dir);
        if (!Files.isDirectory(sourceDir)) {
            throw new IllegalArgumentException("--source-dir " + sourceDir + " is not a directory");
        }
        String modeName = raw.getString(InitialLoadJobConfiguration.MODE);
        LoadMode mode;
        try {
            mode = modeName == null ? properties.mode() : LoadMode.valueOf(modeName.toUpperCase(Locale.ROOT));
        } catch (IllegalArgumentException e) {
            throw new IllegalArgumentException("--mode must be REPLACE or UPSERT, got '" + modeName + "'", e);
        }
        return new JobParametersBuilder(raw)
                .addJobParameters(InitialLoadJobConfiguration.parameters(sourceDir, mode))
                .toJobParameters();
    }
}
