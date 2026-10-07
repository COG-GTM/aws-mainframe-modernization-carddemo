package com.carddemo.batch;

import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.file.RecordFiles;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Clock;
import java.time.LocalDate;
import java.util.HexFormat;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Limit;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * Writes a new generation of a GDG-style output (ADR-0012): {@code <output-dir>/<base>/<base>.<business date>.<job
 * execution id>}, fixed-length records, registered in {@code batch_output_file} with its record count and SHA-256.
 */
@Component
public class DatedOutputFiles {

    private final BatchOutputFileRepository files;
    private final BatchOutputProperties properties;
    private final Clock clock;

    public DatedOutputFiles(BatchOutputFileRepository files, BatchOutputProperties properties, Clock clock) {
        this.files = files;
        this.properties = properties;
        this.clock = clock;
    }

    /** The {@code (+1)} generation of {@code gdgBase}, written and registered. */
    @Transactional
    public BatchOutputFile write(String gdgBase, long jobExecutionId, List<FixedWidthRecord> records) {
        LocalDate businessDate = LocalDate.now(clock);
        Path path = properties.outputDir().resolve(gdgBase)
                .resolve(gdgBase + "." + businessDate + "." + jobExecutionId).toAbsolutePath().normalize();
        RecordFiles.writeFixed(gdgBase, path, records);
        BatchOutputFile saved = files.save(new BatchOutputFile(gdgBase, businessDate, jobExecutionId,
                path.toString(), records.size(), sha256(path)));
        prune(gdgBase);
        return saved;
    }

    /** The file of relative generation {@code (0)}, {@code (-1)}, ... of {@code gdgBase}, if registered. */
    @Transactional(readOnly = true)
    public Optional<Path> generation(String gdgBase, int relative) {
        return files.generation(gdgBase, relative).map(f -> Path.of(f.getFilePath()));
    }

    private void prune(String gdgBase) {
        List<BatchOutputFile> newestFirst =
                files.findByGdgBaseOrderByBusinessDateDescJobExecutionIdDesc(gdgBase, Limit.unlimited());
        for (BatchOutputFile old : newestFirst.subList(Math.min(properties.retain(), newestFirst.size()),
                newestFirst.size())) {
            try {
                Files.deleteIfExists(Path.of(old.getFilePath()));
            } catch (IOException e) {
                throw new UncheckedIOException("cannot delete expired generation " + old.getFilePath(), e);
            }
            files.delete(old);
        }
    }

    public static String sha256(Path path) {
        try (DigestInputStream in = new DigestInputStream(Files.newInputStream(path),
                MessageDigest.getInstance("SHA-256"))) {
            in.transferTo(OutputStream.nullOutputStream());
            return HexFormat.of().formatHex(in.getMessageDigest().digest());
        } catch (IOException e) {
            throw new UncheckedIOException("cannot hash " + path, e);
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException(e);
        }
    }
}
