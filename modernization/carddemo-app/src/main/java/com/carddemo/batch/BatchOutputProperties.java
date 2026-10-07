package com.carddemo.batch;

import java.nio.file.Path;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Where dated batch outputs go (ADR-0012).
 *
 * @param outputDir root directory; each GDG base gets a sub-directory
 * @param retain    generations kept per GDG base ({@code LIMIT(5)} in the DEFGDGB jobs); older files and their
 *                  {@code batch_output_file} rows are removed after each new generation
 */
@ConfigurationProperties("carddemo.batch")
public record BatchOutputProperties(Path outputDir, Integer retain) {

    public BatchOutputProperties {
        outputDir = outputDir == null ? Path.of("batch-output") : outputDir;
        retain = retain == null ? 5 : retain;
        if (retain < 1) {
            throw new IllegalArgumentException("carddemo.batch.retain must be at least 1, got " + retain);
        }
    }
}
