package com.carddemo.programs.paudbunl.io;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;

/** RECFM=FB file: records written back to back with no delimiters. */
public final class FixedWidthFileRecordWriter implements RecordWriter {

    private final Path path;
    private final int lrecl;
    private OutputStream out;

    public FixedWidthFileRecordWriter(Path path, int lrecl) {
        this.path = path;
        this.lrecl = lrecl;
    }

    @Override
    public int recordLength() {
        return lrecl;
    }

    @Override
    public String open() {
        if (out != null) {
            return FileStatus.ALREADY_OPEN;
        }
        try {
            Path parent = path.toAbsolutePath().getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            out = new BufferedOutputStream(Files.newOutputStream(path));
            return FileStatus.SUCCESS;
        } catch (IOException e) {
            return FileStatus.PERMANENT_IO_ERROR;
        }
    }

    @Override
    public String write(byte[] record) {
        if (out == null) {
            return FileStatus.WRITE_NOT_OPEN;
        }
        if (record.length != lrecl) {
            return FileStatus.RECORD_LENGTH_CONFLICT;
        }
        try {
            out.write(record);
            return FileStatus.SUCCESS;
        } catch (IOException e) {
            return FileStatus.PERMANENT_IO_ERROR;
        }
    }

    @Override
    public String close() {
        if (out == null) {
            return FileStatus.NOT_OPEN;
        }
        try {
            out.close();
            return FileStatus.SUCCESS;
        } catch (IOException e) {
            return FileStatus.PERMANENT_IO_ERROR;
        } finally {
            out = null;
        }
    }

    @Override
    public boolean isOpen() {
        return out != null;
    }
}
