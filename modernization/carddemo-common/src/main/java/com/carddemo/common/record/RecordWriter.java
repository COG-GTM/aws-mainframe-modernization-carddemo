package com.carddemo.common.record;

import com.carddemo.common.error.FileStatus;
import com.carddemo.common.error.FileStatusException;

import java.io.BufferedOutputStream;
import java.io.Closeable;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Sequential writer of fixed-length records. {@link RecordFormat#FIXED_BLOCK} writes the images back
 * to back (DALYREJS, 430-byte records); {@link RecordFormat#LINE_SEQUENTIAL} writes each full image
 * followed by {@code \n}. Every image must have the layout's length (FILE STATUS 44 otherwise).
 */
public final class RecordWriter implements Closeable {

    private final RecordLayout layout;
    private final RecordFormat format;
    private final String ddName;
    private final OutputStream out;
    private long recordsWritten;

    private RecordWriter(RecordLayout layout, RecordFormat format, String ddName, OutputStream out) {
        this.layout = layout;
        this.format = format;
        this.ddName = ddName;
        this.out = out;
    }

    /** {@code OPEN OUTPUT}: creates or replaces {@code file}. */
    public static RecordWriter open(Path file, String ddName, RecordLayout layout, RecordFormat format) {
        try {
            return new RecordWriter(layout, format, ddName, new BufferedOutputStream(Files.newOutputStream(file)));
        } catch (IOException ex) {
            throw FileStatusException.of(FileStatus.PERMANENT_ERROR, ddName, "OPEN", ex);
        }
    }

    public static RecordWriter of(OutputStream out, String ddName, RecordLayout layout, RecordFormat format) {
        return new RecordWriter(layout, format, ddName, out);
    }

    public void write(CobolRecord record) {
        write(record.bytes());
    }

    public void write(byte[] image) {
        if (image.length != layout.length()) {
            throw new FileStatusException(FileStatus.WRITE_LENGTH_ERROR, ddName,
                    "WRITE (" + image.length + " bytes, " + layout.copybook() + " is " + layout.length() + ")");
        }
        try {
            out.write(image);
            if (format == RecordFormat.LINE_SEQUENTIAL) {
                out.write('\n');
            }
            recordsWritten++;
        } catch (IOException ex) {
            throw FileStatusException.of(FileStatus.PERMANENT_ERROR, ddName, "WRITE", ex);
        }
    }

    public long recordsWritten() {
        return recordsWritten;
    }

    @Override
    public void close() {
        try {
            out.close();
        } catch (IOException ex) {
            throw FileStatusException.of(FileStatus.PERMANENT_ERROR, ddName, "CLOSE", ex);
        }
    }
}
