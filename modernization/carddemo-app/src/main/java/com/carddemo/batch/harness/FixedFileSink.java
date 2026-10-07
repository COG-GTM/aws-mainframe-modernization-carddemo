package com.carddemo.batch.harness;

import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.file.FileStatus;
import com.carddemo.common.file.FileStatusException;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.AccessDeniedException;
import java.nio.file.Files;
import java.nio.file.Path;

/** RECFM=FB: each record's codec bytes back to back; every record must have the length of the first. */
public final class FixedFileSink implements RecordSink {

    private final String ddname;
    private final Path path;
    private OutputStream out;
    private int lrecl = -1;
    private long count;

    public FixedFileSink(String ddname, Path path) {
        this.ddname = ddname;
        this.path = path;
    }

    @Override
    public String ddname() {
        return ddname;
    }

    public Path path() {
        return path;
    }

    @Override
    public void open() {
        if (out != null) {
            throw new FileStatusException(ddname, "OPEN", FileStatus.ALREADY_OPEN);
        }
        try {
            Path parent = path.toAbsolutePath().getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            out = new BufferedOutputStream(Files.newOutputStream(path));
        } catch (AccessDeniedException e) {
            throw new FileStatusException(ddname, "OPEN", FileStatus.OPEN_MODE_NOT_ALLOWED, e);
        } catch (IOException e) {
            throw new FileStatusException(ddname, "OPEN", FileStatus.PERMANENT_ERROR, e);
        }
        count = 0;
    }

    @Override
    public void write(FixedWidthRecord record) {
        if (out == null) {
            throw new FileStatusException(ddname, "WRITE", FileStatus.NOT_OPEN_OUTPUT);
        }
        byte[] bytes = record.bytes();
        if (lrecl < 0) {
            lrecl = bytes.length;
        } else if (bytes.length != lrecl) {
            throw new FileStatusException(ddname, "WRITE", FileStatus.RECORD_LENGTH_ERROR);
        }
        try {
            out.write(bytes);
        } catch (IOException e) {
            throw new FileStatusException(ddname, "WRITE", FileStatus.PERMANENT_ERROR, e);
        }
        count++;
    }

    @Override
    public long count() {
        return count;
    }

    @Override
    public boolean isOpen() {
        return out != null;
    }

    @Override
    public void close() {
        if (out == null) {
            return;
        }
        try {
            out.close();
        } catch (IOException e) {
            throw new FileStatusException(ddname, "CLOSE", FileStatus.PERMANENT_ERROR, e);
        } finally {
            out = null;
        }
    }
}
