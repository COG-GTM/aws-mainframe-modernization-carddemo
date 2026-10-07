package com.carddemo.common.file;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.AccessDeniedException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;

/**
 * Writes variable-length records ({@code RECORDING MODE IS V ... DEPENDING ON WS-RECD-LEN}) with the chosen
 * {@link RecordPrefix}. Each WRITE emits exactly the first {@code length} bytes of the record area. Failures
 * surface as {@link FileStatusException} with the status the COBOL program would have seen.
 */
public final class VariableRecordWriter {

    private final String ddname;
    private final Path path;
    /** The two-byte RDW length includes its own four bytes. */
    public static final int MAX_RDW_PAYLOAD = 0xFFFF - 4;

    private final RecordPrefix prefix;
    private final int minLength;
    private final int maxLength;
    private OutputStream out;

    public VariableRecordWriter(String ddname, Path path, RecordPrefix prefix, int minLength, int maxLength) {
        if (minLength < 0 || maxLength < minLength) {
            throw new IllegalArgumentException("record length range " + minLength + ".." + maxLength);
        }
        if (prefix == RecordPrefix.ZOS_RDW && maxLength > MAX_RDW_PAYLOAD) {
            throw new IllegalArgumentException("an RDW holds at most " + MAX_RDW_PAYLOAD + " payload bytes");
        }
        if (prefix == RecordPrefix.GNUCOBOL_VARSEQ_0 && maxLength > 0xFFFF) {
            throw new IllegalArgumentException("COB_VARSEQ_FORMAT=0 holds at most 65535 payload bytes");
        }
        this.ddname = ddname;
        this.path = path;
        this.prefix = prefix;
        this.minLength = minLength;
        this.maxLength = maxLength;
    }

    public void open() {
        if (out != null) {
            throw new FileStatusException(ddname, "OPEN", FileStatus.ALREADY_OPEN);
        }
        try {
            out = new BufferedOutputStream(Files.newOutputStream(path));
        } catch (NoSuchFileException e) {
            throw new FileStatusException(ddname, "OPEN", FileStatus.FILE_NOT_FOUND, e);
        } catch (AccessDeniedException e) {
            throw new FileStatusException(ddname, "OPEN", FileStatus.OPEN_MODE_NOT_ALLOWED, e);
        } catch (IOException e) {
            throw new FileStatusException(ddname, "OPEN", FileStatus.PERMANENT_ERROR, e);
        }
    }

    public void write(byte[] recordArea, int length) {
        if (out == null) {
            throw new FileStatusException(ddname, "WRITE", FileStatus.NOT_OPEN_OUTPUT);
        }
        if (length < minLength || length > maxLength || length > recordArea.length) {
            throw new FileStatusException(ddname, "WRITE", FileStatus.RECORD_LENGTH_ERROR,
                    new IOException("record length " + length + " outside " + minLength + ".." + maxLength));
        }
        try {
            switch (prefix) {
                case GNUCOBOL_VARSEQ -> {
                    out.write((length >>> 24) & 0xFF);
                    out.write((length >>> 16) & 0xFF);
                    out.write((length >>> 8) & 0xFF);
                    out.write(length & 0xFF);
                }
                case GNUCOBOL_VARSEQ_0 -> {
                    out.write((length >>> 8) & 0xFF);
                    out.write(length & 0xFF);
                    out.write(0);
                    out.write(0);
                }
                case ZOS_RDW -> {
                    int rdw = length + 4;
                    out.write((rdw >>> 8) & 0xFF);
                    out.write(rdw & 0xFF);
                    out.write(0);
                    out.write(0);
                }
                case NONE -> {
                    // payload only
                }
            }
            out.write(recordArea, 0, length);
        } catch (IOException e) {
            throw new FileStatusException(ddname, "WRITE", FileStatus.PERMANENT_ERROR, e);
        }
    }

    public boolean isOpen() {
        return out != null;
    }

    public void close() {
        if (out == null) {
            throw new FileStatusException(ddname, "CLOSE", FileStatus.NOT_OPEN);
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
