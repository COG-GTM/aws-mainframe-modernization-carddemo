package com.carddemo.programs.cbstm03a.io;

import com.carddemo.programs.cbstm03a.support.Pic;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Writes fixed-length records. {@code lineDelimited=false} reproduces RECFM=FB bytes
 * exactly (no separators); {@code true} appends a newline after every record.
 */
public final class FixedWidthFileWriter implements RecordWriter {

    private final int recordLength;
    private final boolean lineDelimited;
    private final BufferedWriter out;

    public FixedWidthFileWriter(Path path, int recordLength, boolean lineDelimited) {
        this.recordLength = recordLength;
        this.lineDelimited = lineDelimited;
        try {
            this.out = Files.newBufferedWriter(path, StandardCharsets.ISO_8859_1);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public void write(String record) {
        try {
            out.write(Pic.x(record, recordLength));
            if (lineDelimited) {
                out.write('\n');
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
