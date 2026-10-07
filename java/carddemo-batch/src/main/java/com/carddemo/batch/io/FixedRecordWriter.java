package com.carddemo.batch.io;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;

/** Writes fixed-length (RECFM=FB) records back to back, exactly as the COBOL WRITE does. */
public final class FixedRecordWriter {

    private final String ddname;
    private final Path path;
    private final int recordLength;
    private OutputStream out;

    public FixedRecordWriter(String ddname, Path path, int recordLength) {
        this.ddname = ddname;
        this.path = path;
        this.recordLength = recordLength;
    }

    public void open() {
        try {
            out = new BufferedOutputStream(Files.newOutputStream(path));
        } catch (NoSuchFileException e) {
            throw new FileStatusException(ddname, "OPEN", FileStatusException.FILE_NOT_FOUND, e);
        } catch (IOException e) {
            throw new FileStatusException(ddname, "OPEN", FileStatusException.PERMANENT_ERROR, e);
        }
    }

    public void write(byte[] record) {
        if (out == null) {
            throw new FileStatusException(ddname, "WRITE", FileStatusException.NOT_OPEN);
        }
        if (record.length != recordLength) {
            throw new FileStatusException(ddname, "WRITE", FileStatusException.ATTRIBUTE_MISMATCH,
                    new IOException("record is " + record.length + " bytes, LRECL is " + recordLength));
        }
        try {
            out.write(record);
        } catch (IOException e) {
            throw new FileStatusException(ddname, "WRITE", FileStatusException.PERMANENT_ERROR, e);
        }
    }

    public boolean isOpen() {
        return out != null;
    }

    public void close() {
        if (out == null) {
            throw new FileStatusException(ddname, "CLOSE", FileStatusException.NOT_OPEN);
        }
        try {
            out.close();
        } catch (IOException e) {
            throw new FileStatusException(ddname, "CLOSE", FileStatusException.PERMANENT_ERROR, e);
        } finally {
            out = null;
        }
    }
}
