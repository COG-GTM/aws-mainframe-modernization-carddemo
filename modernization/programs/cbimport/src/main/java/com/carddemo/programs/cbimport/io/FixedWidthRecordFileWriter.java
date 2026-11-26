package com.carddemo.programs.cbimport.io;

import com.carddemo.programs.cbimport.FileStatus;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * RECFM=FB output file. Records are encoded with {@code charset} (IBM037 for mainframe-identical
 * bytes, US-ASCII for the app/data/ASCII form) and followed by {@code recordSeparator} (empty for
 * true FB, "\n" for line-sequential).
 */
public final class FixedWidthRecordFileWriter implements RecordWriter {

    private final Path path;
    private final int recordLength;
    private final Charset charset;
    private final byte[] separator;
    private OutputStream out;

    public FixedWidthRecordFileWriter(Path path, int recordLength, Charset charset, String recordSeparator) {
        this.path = path;
        this.recordLength = recordLength;
        this.charset = charset;
        this.separator = recordSeparator.getBytes(charset);
    }

    public Path path() {
        return path;
    }

    @Override
    public String open() {
        try {
            out = new BufferedOutputStream(Files.newOutputStream(path));
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    @Override
    public String write(String record) {
        if (record.length() != recordLength) {
            return FileStatus.BOUNDARY_VIOLATION;
        }
        try {
            out.write(record.getBytes(charset));
            out.write(separator);
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    @Override
    public String close() {
        try {
            if (out != null) {
                out.close();
            }
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }
}
