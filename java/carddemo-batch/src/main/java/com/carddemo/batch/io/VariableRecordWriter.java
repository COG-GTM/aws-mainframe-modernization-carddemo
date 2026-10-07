package com.carddemo.batch.io;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;

/**
 * Writes variable-length records ({@code RECORDING MODE IS V ... DEPENDING ON WS-RECD-LEN}) with the
 * chosen {@link RecordPrefix}. Each WRITE emits exactly the first {@code length} bytes of the record area.
 */
public final class VariableRecordWriter {

    private final String ddname;
    private final Path path;
    private final RecordPrefix prefix;
    private final int minLength;
    private final int maxLength;
    private OutputStream out;

    public VariableRecordWriter(String ddname, Path path, RecordPrefix prefix, int minLength, int maxLength) {
        this.ddname = ddname;
        this.path = path;
        this.prefix = prefix;
        this.minLength = minLength;
        this.maxLength = maxLength;
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

    public void write(byte[] recordArea, int length) {
        if (out == null) {
            throw new FileStatusException(ddname, "WRITE", FileStatusException.NOT_OPEN);
        }
        if (length < minLength || length > maxLength || length > recordArea.length) {
            throw new FileStatusException(ddname, "WRITE", FileStatusException.ATTRIBUTE_MISMATCH,
                    new IOException("record length " + length + " outside " + minLength + ".." + maxLength));
        }
        try {
            switch (prefix) {
                case GNUCOBOL_VARSEQ:
                    out.write((length >>> 24) & 0xFF);
                    out.write((length >>> 16) & 0xFF);
                    out.write((length >>> 8) & 0xFF);
                    out.write(length & 0xFF);
                    break;
                case ZOS_RDW:
                    int rdw = length + 4;
                    out.write((rdw >>> 8) & 0xFF);
                    out.write(rdw & 0xFF);
                    out.write(0);
                    out.write(0);
                    break;
                case NONE:
                default:
                    break;
            }
            out.write(recordArea, 0, length);
        } catch (IOException e) {
            throw new FileStatusException(ddname, "WRITE", FileStatusException.PERMANENT_ERROR, e);
        }
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
