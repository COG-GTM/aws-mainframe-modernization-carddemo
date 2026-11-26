package com.carddemo.programs.cbexport.io;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;

/**
 * Writes RECFM=F records with no delimiters. When a key position is given it behaves like a
 * KSDS loaded with ACCESS MODE SEQUENTIAL: keys must be strictly ascending (unsigned byte
 * comparison) or the WRITE returns status 21.
 */
public final class FixedLengthRecordWriter implements RecordWriter {

    private final Path path;
    private final int recordLength;
    private final int keyOffset;
    private final int keyLength;
    private byte[] lastKey;
    private OutputStream out;

    public FixedLengthRecordWriter(Path path, int recordLength) {
        this(path, recordLength, 0, 0);
    }

    public FixedLengthRecordWriter(Path path, int recordLength, int keyOffset, int keyLength) {
        this.path = path;
        this.recordLength = recordLength;
        this.keyOffset = keyOffset;
        this.keyLength = keyLength;
    }

    @Override
    public String open() {
        try {
            out = Files.newOutputStream(path);
            lastKey = null;
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    @Override
    public String write(byte[] record) {
        if (record.length != recordLength) {
            return FileStatus.RECORD_LENGTH_ERROR;
        }
        if (keyLength > 0) {
            byte[] key = Arrays.copyOfRange(record, keyOffset, keyOffset + keyLength);
            if (lastKey != null && Arrays.compareUnsigned(key, lastKey) <= 0) {
                return FileStatus.SEQUENCE_ERROR;
            }
            lastKey = key;
        }
        try {
            out.write(record);
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    @Override
    public void close() {
        if (out != null) {
            try {
                out.close();
            } catch (IOException ignored) {
                // CLOSE status is not checked by CBEXPORT.
            }
            out = null;
        }
    }
}
