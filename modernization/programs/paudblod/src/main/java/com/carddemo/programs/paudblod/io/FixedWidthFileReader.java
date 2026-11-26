package com.carddemo.programs.paudblod.io;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.Arrays;

/** RECFM=FB reader: fixed-length binary records with no delimiters (packed fields are kept as bytes). */
public final class FixedWidthFileReader implements SequentialRecordReader {

    private final Path path;
    private final int recordLength;
    private InputStream in;

    public FixedWidthFileReader(Path path, int recordLength) {
        this.path = path;
        this.recordLength = recordLength;
    }

    @Override
    public String open() {
        try {
            in = Files.newInputStream(path);
            return FileStatus.SUCCESS;
        } catch (NoSuchFileException e) {
            return FileStatus.FILE_NOT_FOUND;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    @Override
    public ReadResult read() {
        if (in == null) {
            return ReadResult.error(FileStatus.NOT_OPEN);
        }
        try {
            byte[] buf = in.readNBytes(recordLength);
            if (buf.length == 0) {
                return ReadResult.endOfFile();
            }
            if (buf.length < recordLength) {
                return new ReadResult(FileStatus.RECORD_LENGTH_MISMATCH, Arrays.copyOf(buf, recordLength));
            }
            return ReadResult.ok(buf);
        } catch (IOException e) {
            return ReadResult.error(FileStatus.PERMANENT_ERROR);
        }
    }

    @Override
    public String close() {
        if (in == null) {
            return FileStatus.NOT_OPEN;
        }
        try {
            in.close();
            in = null;
            return FileStatus.SUCCESS;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }
}
