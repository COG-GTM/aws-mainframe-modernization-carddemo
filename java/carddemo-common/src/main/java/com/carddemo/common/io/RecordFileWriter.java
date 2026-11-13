package com.carddemo.common.io;

import com.carddemo.common.cobol.CobolDataException;

import java.io.BufferedOutputStream;
import java.io.Closeable;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;

/** Writes records in a mainframe-compatible physical format (the Java side of a COBOL {@code WRITE}). */
public final class RecordFileWriter implements Closeable {

    private final OutputStream out;
    private final RecordFormat format;
    private final int lrecl;

    public RecordFileWriter(OutputStream out, RecordFormat format, int lrecl) {
        this.out = new BufferedOutputStream(out);
        this.format = format;
        this.lrecl = lrecl;
    }

    public static RecordFileWriter create(Path path, RecordFormat format, int lrecl) {
        try {
            return new RecordFileWriter(Files.newOutputStream(path), format, lrecl);
        } catch (IOException e) {
            throw new UncheckedIOException("Cannot create " + path, e);
        }
    }

    public void write(byte[] record) {
        try {
            switch (format) {
                case FIXED -> {
                    if (record.length != lrecl) {
                        throw new CobolDataException("Record of " + record.length + " bytes in a FIXED file of LRECL " + lrecl);
                    }
                    out.write(record);
                }
                case VARIABLE -> {
                    if (record.length > lrecl) {
                        throw new CobolDataException("Record of " + record.length + " bytes exceeds LRECL " + lrecl);
                    }
                    int length = record.length + 4;
                    out.write(new byte[] {(byte) (length >>> 8), (byte) length, 0, 0});
                    out.write(record);
                }
                case LINE -> {
                    out.write(record);
                    out.write('\n');
                }
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public void close() {
        try {
            out.close();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
