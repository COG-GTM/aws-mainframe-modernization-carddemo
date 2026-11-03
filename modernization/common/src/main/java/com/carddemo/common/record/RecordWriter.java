package com.carddemo.common.record;

import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.Flushable;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

/** Sequential writer of fixed-width records, the reverse of {@link RecordReader}. */
public final class RecordWriter implements Closeable, Flushable {

    private final OutputStream out;
    private final RecordFormat format;
    private final byte[] lineEnding;

    public RecordWriter(OutputStream out, RecordFormat format) {
        this.out = out;
        this.format = format;
        this.lineEnding = format.lineEnding().getBytes(StandardCharsets.US_ASCII);
    }

    public static void writeAll(Path file, Iterable<CobolRecord> records, RecordFormat format) throws IOException {
        try (RecordWriter writer = new RecordWriter(Files.newOutputStream(file), format)) {
            for (CobolRecord record : records) {
                writer.write(record);
            }
        }
    }

    /** The file content {@link #writeAll} would produce. */
    public static byte[] toBytes(Iterable<CobolRecord> records, RecordFormat format) {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        try (RecordWriter writer = new RecordWriter(buffer, format)) {
            for (CobolRecord record : records) {
                writer.write(record);
            }
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
        return buffer.toByteArray();
    }

    public void write(CobolRecord record) throws IOException {
        write(record, format.lineEnding());
    }

    /** Writes a line-sequential record with an explicit terminator (see {@link RecordReader#lineEnding()}). */
    public void write(CobolRecord record, String terminator) throws IOException {
        byte[] bytes = record.toBytes();
        if (format.organization() == RecordFormat.Organization.FIXED) {
            out.write(bytes);
            return;
        }
        if (record.codePage() != CodePage.ASCII) {
            throw new IllegalArgumentException("Line-sequential files are ASCII");
        }
        int length = bytes.length;
        if (format.trimTrailingSpaces()) {
            byte space = record.codePage().space();
            while (length > 0 && bytes[length - 1] == space) {
                length--;
            }
        }
        out.write(bytes, 0, length);
        out.write(terminator.equals(format.lineEnding()) ? lineEnding : terminator.getBytes(StandardCharsets.US_ASCII));
    }

    @Override
    public void flush() throws IOException {
        out.flush();
    }

    @Override
    public void close() throws IOException {
        out.close();
    }
}
