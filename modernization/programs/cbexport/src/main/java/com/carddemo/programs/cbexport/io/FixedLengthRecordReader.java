package com.carddemo.programs.cbexport.io;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;

/** Reads RECFM=F records: consecutive blocks of exactly {@code recordLength} bytes, no delimiters. */
public final class FixedLengthRecordReader implements RecordReader {

    private final Path path;
    private final int recordLength;
    private InputStream in;

    public FixedLengthRecordReader(Path path, int recordLength) {
        this.path = path;
        this.recordLength = recordLength;
    }

    @Override
    public String open() {
        if (!Files.isRegularFile(path)) {
            return FileStatus.FILE_NOT_FOUND;
        }
        try {
            in = Files.newInputStream(path);
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    @Override
    public ReadResult read() {
        try {
            byte[] rec = in.readNBytes(recordLength);
            if (rec.length == 0) {
                return ReadResult.status(FileStatus.EOF);
            }
            if (rec.length != recordLength) {
                return ReadResult.status(FileStatus.LENGTH_MISMATCH);
            }
            return ReadResult.ok(rec);
        } catch (IOException e) {
            return ReadResult.status(FileStatus.PERMANENT_ERROR);
        }
    }

    @Override
    public void close() {
        if (in != null) {
            try {
                in.close();
            } catch (IOException ignored) {
                // CLOSE status is not checked by CBEXPORT.
            }
            in = null;
        }
    }
}
