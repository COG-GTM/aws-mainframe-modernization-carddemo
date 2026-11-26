package com.carddemo.programs.cbact01c.io;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;

/** RECFM=FB sequential output: records of exactly {@code lrecl} bytes, written back to back with no delimiters. */
public final class FixedRecordFileWriter implements RecordWriter {
    private final Path path;
    private final int lrecl;
    private OutputStream out;

    public FixedRecordFileWriter(Path path, int lrecl) {
        this.path = path;
        this.lrecl = lrecl;
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
    public String write(byte[] record) {
        if (out == null) {
            return FileStatus.NOT_OPEN_FOR_WRITE;
        }
        if (record.length != lrecl) {
            return FileStatus.RECORD_SIZE_INVALID;
        }
        try {
            out.write(record);
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    @Override
    public String close() {
        if (out == null) {
            return FileStatus.NOT_OPEN_FOR_CLOSE;
        }
        try {
            out.close();
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        } finally {
            out = null;
        }
    }
}
