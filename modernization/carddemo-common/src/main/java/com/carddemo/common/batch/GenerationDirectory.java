package com.carddemo.common.batch;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * GDG {@code (+1)} replacement: one new directory per run, {@code <output-dir>/<JOB>/<yyyyMMdd-HHmmss-SSS>} (UTC),
 * holding the job's output datasets, {@code SYSOUT} and {@code RETURN-CODE}. Directory names sort
 * chronologically (UTC has no DST step back), so the latest generation is the greatest name ({@code (0)}). A dataset that must
 * not exist after an abend ({@code DISP=(NEW,CATLG,DELETE)}) is written to a temp file and only
 * {@link #publish published} on success.
 */
public final class GenerationDirectory {

    public static final String SYSOUT = "SYSOUT";
    public static final String RETURN_CODE = "RETURN-CODE";
    private static final DateTimeFormatter NAME = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss-SSS");

    private final Path path;

    private GenerationDirectory(Path path) {
        this.path = path;
    }

    /** Creates the next generation; if the millisecond name is taken, the next free millisecond is used. */
    public static GenerationDirectory create(Path outputDir, String job, Clock clock) {
        Path jobDir = outputDir.resolve(job);
        try {
            Files.createDirectories(jobDir);
            Instant instant = clock.instant();
            for (int attempt = 0; attempt < 1000; attempt++) {
                Path candidate = jobDir.resolve(NAME.format(instant.plusMillis(attempt).atZone(ZoneOffset.UTC)));
                try {
                    return new GenerationDirectory(Files.createDirectory(candidate));
                } catch (FileAlreadyExistsException ignored) {
                    // next millisecond
                }
            }
            throw new IOException("No free generation name under " + jobDir);
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    /** The latest existing generation of {@code job} ({@code (0)}), if any. */
    public static Optional<Path> latest(Path outputDir, String job) {
        Path jobDir = outputDir.resolve(job);
        if (!Files.isDirectory(jobDir)) {
            return Optional.empty();
        }
        try (Stream<Path> children = Files.list(jobDir)) {
            return children.filter(Files::isDirectory)
                    .filter(p -> p.getFileName().toString().matches("\\d{8}-\\d{6}-\\d{3}"))
                    .max(Path::compareTo);
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    public Path path() {
        return path;
    }

    public Path resolve(String dataset) {
        return path.resolve(dataset);
    }

    /** Hidden temp file a dataset is written to before {@link #publish}. */
    public Path temp(String dataset) {
        return path.resolve("." + dataset + ".tmp");
    }

    /** Atomically renames the temp file to the dataset name (catalogs the dataset). */
    public Path publish(String dataset) {
        try {
            return Files.move(temp(dataset), resolve(dataset), StandardCopyOption.ATOMIC_MOVE);
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    /** Deletes the temp file of an unpublished dataset (the {@code DELETE} of an abend disposition). */
    public void discard(String dataset) {
        try {
            Files.deleteIfExists(temp(dataset));
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    /** Writes {@code RETURN-CODE} as the decimal RC and a newline (same as the GnuCOBOL oracle script). */
    public Path writeReturnCode(int returnCode) {
        try {
            return Files.writeString(resolve(RETURN_CODE), ReturnCode.validate(returnCode) + "\n", StandardCharsets.US_ASCII);
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }
}
