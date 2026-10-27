package com.carddemo.modernization.batch;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.batch.item.ItemStreamReader;

/** Streams a sequential dataset one fixed-length record at a time; a short final record fails the step. */
final class FixedLengthRecordReader implements ItemStreamReader<byte[]> {
    private final Path file;
    private final int length;
    private InputStream in;
    private long records;

    FixedLengthRecordReader(Path file, int length) {
        this.file = file;
        this.length = length;
    }

    @Override
    public void open(ExecutionContext context) {
        try {
            in = Files.newInputStream(file);
        } catch (IOException e) {
            throw new ItemStreamException("cannot open " + file, e);
        }
    }

    @Override
    public byte[] read() {
        try {
            byte[] record = in.readNBytes(length);
            if (record.length == 0) {
                return null;
            }
            if (record.length != length) {
                throw new IllegalStateException(file + " record " + (records + 1) + " is " + record.length
                        + " bytes, expected LRECL " + length);
            }
            records++;
            return record;
        } catch (IOException e) {
            throw new UncheckedIOException("cannot read " + file, e);
        }
    }

    @Override
    public void close() {
        try {
            if (in != null) {
                in.close();
            }
        } catch (IOException e) {
            throw new ItemStreamException("cannot close " + file, e);
        }
    }
}
