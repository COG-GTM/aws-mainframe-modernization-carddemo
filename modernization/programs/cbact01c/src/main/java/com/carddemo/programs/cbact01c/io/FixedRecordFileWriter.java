package com.carddemo.programs.cbact01c.io;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

/** RECFM=FB sequential output: records of exactly {@code lrecl} bytes, written back to back with no delimiters. */
public final class FixedRecordFileWriter implements RecordWriter {
    private final Path path;
    private final int lrecl;
    private OutputStream out;
    private boolean created;

    public FixedRecordFileWriter(Path path, int lrecl) {
        this.path = path;
        this.lrecl = lrecl;
    }

    @Override
    public String open() {
        try {
            // DISP=NEW: never replace or follow an existing file or link.
            out = new BufferedOutputStream(
                    Files.newOutputStream(path, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE));
            created = true;
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    /** True once {@link #open()} has created the file, so only this job's output is deleted on abend. */
    public boolean created() {
        return created;
    }

    public Path path() {
        return path;
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
