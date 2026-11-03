package com.carddemo.common.batch;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Comparator;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * The GDG stand-in. Each run of a job gets a new generation directory
 * {@code <root>/<JOB>/<yyyyMMdd-HHmmss-SSS>} (UTC) that holds one file per output DD, named after
 * the DD ({@code DALYREJS}, {@code TRANSACT}, ...), plus {@code RETURN-CODE}. A {@code (+1)} DSN is a
 * file in the new generation; {@code (0)} is the same DD in the latest earlier generation.
 */
public final class JobRunDirectory {

    public static final String RETURN_CODE = "RETURN-CODE";
    static final DateTimeFormatter STAMP = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss-SSS").withZone(ZoneOffset.UTC);

    private final Path path;

    private JobRunDirectory(Path path) {
        this.path = path;
    }

    public static JobRunDirectory of(Path path) {
        return new JobRunDirectory(path);
    }

    /** Creates the next generation directory; a second run within the same millisecond gets a suffix. */
    public static JobRunDirectory create(Path root, String job, Instant now) {
        Path jobDir = root.resolve(job);
        String stamp = STAMP.format(now);
        try {
            Files.createDirectories(jobDir);
            for (int n = 0; ; n++) {
                Path candidate = jobDir.resolve(n == 0 ? stamp : stamp + "-" + n);
                try {
                    return new JobRunDirectory(Files.createDirectory(candidate));
                } catch (java.nio.file.FileAlreadyExistsException taken) {
                    // next suffix
                }
            }
        } catch (IOException ex) {
            throw new UncheckedIOException("Cannot create output generation under " + jobDir, ex);
        }
    }

    /** {@code DSN=...(0)}: the DD file of the newest generation of {@code job} that has one. */
    public static Optional<Path> latestGeneration(Path root, String job, String ddName) {
        Path jobDir = root.resolve(job);
        if (!Files.isDirectory(jobDir)) {
            return Optional.empty();
        }
        try (Stream<Path> generations = Files.list(jobDir)) {
            return generations.filter(Files::isDirectory)
                    .sorted(Comparator.comparing((Path p) -> p.getFileName().toString()).reversed())
                    .flatMap(generation -> Stream.of(generation.resolve(ddName), generation.resolve(ddName + ".txt"))
                            .filter(Files::isRegularFile)
                            .limit(1))
                    .findFirst();
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    public Path path() {
        return path;
    }

    /** The file of output DD {@code ddName} in this generation. */
    public Path dd(String ddName) {
        return path.resolve(ddName);
    }

    public void writeReturnCode(int returnCode) {
        try {
            Files.writeString(dd(RETURN_CODE), returnCode + "\n", StandardCharsets.US_ASCII);
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }
}
