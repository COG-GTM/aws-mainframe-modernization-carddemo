package com.carddemo.programs.cbexport.io;

import com.carddemo.programs.cbexport.copybook.CobolFieldCodec;
import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;

/**
 * Reads newline-delimited fixed-width records (the app/data/ASCII rendering of the VSAM files).
 * Short lines are space-padded to the copybook length, as LINE SEQUENTIAL reads do; a line longer
 * than the record returns status 04.
 */
public final class LineSequentialRecordReader implements RecordReader {

    private final Path path;
    private final int recordLength;
    private BufferedReader in;

    public LineSequentialRecordReader(Path path, int recordLength) {
        this.path = path;
        this.recordLength = recordLength;
    }

    @Override
    public String open() {
        if (!Files.isRegularFile(path)) {
            return FileStatus.FILE_NOT_FOUND;
        }
        try {
            in = Files.newBufferedReader(path, CobolFieldCodec.CHARSET);
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    @Override
    public ReadResult read() {
        try {
            // Buffers at most recordLength + 1 characters (the +1 allows a CR before LF).
            char[] buf = new char[recordLength + 1];
            int n = 0;
            boolean overflow = false;
            int c = in.read();
            if (c < 0) {
                return ReadResult.status(FileStatus.EOF);
            }
            while (c >= 0 && c != '\n') {
                if (n < buf.length) {
                    buf[n++] = (char) c;
                } else {
                    overflow = true;
                }
                c = in.read();
            }
            if (n > 0 && buf[n - 1] == '\r') {
                n--;
            }
            if (overflow || n > recordLength) {
                return ReadResult.status(FileStatus.LENGTH_MISMATCH);
            }
            byte[] src = new String(buf, 0, n).getBytes(CobolFieldCodec.CHARSET);
            byte[] rec = new byte[recordLength];
            Arrays.fill(rec, CobolFieldCodec.SPACE);
            System.arraycopy(src, 0, rec, 0, src.length);
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
