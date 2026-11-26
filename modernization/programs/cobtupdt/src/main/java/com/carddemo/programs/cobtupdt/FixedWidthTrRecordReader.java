package com.carddemo.programs.cobtupdt;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.UncheckedIOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;

/**
 * Reads 53-byte TR-RECORD records (RECORDING MODE F) from a text source.
 *
 * <ul>
 *   <li>{@link Layout#LINE_SEQUENTIAL}: one record per line (CR/LF stripped), as in app/data/ASCII.</li>
 *   <li>{@link Layout#FIXED_BLOCK}: records are consecutive 53-character blocks with no separators.</li>
 * </ul>
 * Records are returned as read; {@link TrRecord#parse(String)} pads/truncates them to 53 bytes.
 */
public final class FixedWidthTrRecordReader implements TrRecordReader {

    public enum Layout { LINE_SEQUENTIAL, FIXED_BLOCK }

    @FunctionalInterface
    public interface ReaderSource {
        Reader open() throws IOException;
    }

    private final ReaderSource source;
    private final Layout layout;
    private BufferedReader reader;

    public FixedWidthTrRecordReader(ReaderSource source, Layout layout) {
        this.source = source;
        this.layout = layout;
    }

    public static FixedWidthTrRecordReader of(Path path) {
        return of(path, StandardCharsets.UTF_8, Layout.LINE_SEQUENTIAL);
    }

    public static FixedWidthTrRecordReader of(Path path, Charset charset, Layout layout) {
        return new FixedWidthTrRecordReader(() -> Files.newBufferedReader(path, charset), layout);
    }

    public static FixedWidthTrRecordReader ofLines(List<String> lines) {
        return new FixedWidthTrRecordReader(() -> new StringReader(String.join("\n", lines)
                + (lines.isEmpty() ? "" : "\n")), Layout.LINE_SEQUENTIAL);
    }

    @Override
    public String open() {
        if (reader != null) {
            return "41";
        }
        try {
            reader = new BufferedReader(source.open());
            return STATUS_OK;
        } catch (NoSuchFileException e) {
            return STATUS_FILE_NOT_FOUND;
        } catch (IOException e) {
            return "37";
        }
    }

    @Override
    public Optional<String> readNext() {
        if (reader == null) {
            throw new CobolFileStatusException(STATUS_NOT_OPEN, "READ TR-RECORD on a file that is not open");
        }
        try {
            return layout == Layout.LINE_SEQUENTIAL ? readLine() : readBlock();
        } catch (IOException e) {
            throw new CobolFileStatusException("30", "READ TR-RECORD failed", e);
        }
    }

    private Optional<String> readLine() throws IOException {
        String line = reader.readLine();
        if (line == null) {
            return Optional.empty();
        }
        if (line.endsWith("\r")) {
            line = line.substring(0, line.length() - 1);
        }
        return Optional.of(line);
    }

    private Optional<String> readBlock() throws IOException {
        char[] buf = new char[TrRecord.RECORD_LENGTH];
        int n = 0;
        while (n < buf.length) {
            int r = reader.read(buf, n, buf.length - n);
            if (r < 0) {
                break;
            }
            n += r;
        }
        return n == 0 ? Optional.empty() : Optional.of(new String(buf, 0, n));
    }

    @Override
    public String close() {
        if (reader == null) {
            return "42";
        }
        try {
            reader.close();
            return STATUS_OK;
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        } finally {
            reader = null;
        }
    }
}
