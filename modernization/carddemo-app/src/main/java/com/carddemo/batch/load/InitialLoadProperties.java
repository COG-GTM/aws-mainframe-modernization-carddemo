package com.carddemo.batch.load;

import java.nio.file.Files;
import java.nio.file.Path;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Settings of the {@code initial-load} job.
 *
 * @param onStartup  launch the job when the application starts ({@code local} profile); a completed run for the same
 *                   source files is not repeated
 * @param sourceDir  directory holding the {@code AWS.M2.CARDDEMO.*.PS} EBCDIC images; a relative path is looked up
 *                   from the working directory and its parents, so it works from the repo root and from
 *                   {@code modernization/}
 * @param mode       {@code replace} (truncate-and-load) or {@code upsert}
 * @param maxRejects records per dataset that may fail to map before the step fails
 */
@ConfigurationProperties("carddemo.initial-load")
public record InitialLoadProperties(boolean onStartup, Path sourceDir, LoadMode mode, int maxRejects) {

    public InitialLoadProperties {
        sourceDir = sourceDir == null ? Path.of("app/data/EBCDIC") : sourceDir;
        mode = mode == null ? LoadMode.REPLACE : mode;
    }

    public Path resolvedSourceDir() {
        if (sourceDir.isAbsolute()) {
            return sourceDir;
        }
        for (Path dir = Path.of("").toAbsolutePath(); dir != null; dir = dir.getParent()) {
            Path candidate = dir.resolve(sourceDir);
            if (Files.isDirectory(candidate)) {
                return candidate.normalize();
            }
        }
        return sourceDir.toAbsolutePath();
    }
}
