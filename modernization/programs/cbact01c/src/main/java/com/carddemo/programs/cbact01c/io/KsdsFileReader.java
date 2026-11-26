package com.carddemo.programs.cbact01c.io;

import com.carddemo.programs.cbact01c.support.RecordText;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * Sequential reader over a VSAM KSDS unloaded as a text file (one record per line, as in app/data/ASCII).
 * Records are returned in ascending key order ({@code RECORD KEY} = first {@code keyLength} bytes), as a KSDS
 * read sequentially would return them. Lines shorter than the record length are space-padded (trailing
 * blanks are often trimmed in text exports); longer lines give status 04.
 */
public final class KsdsFileReader implements SequentialRecordReader {
    private final Path path;
    private final int recordLength;
    private final int keyLength;
    private List<byte[]> records;
    private int next;
    private boolean eof;

    public KsdsFileReader(Path path, int recordLength, int keyLength) {
        this.path = path;
        this.recordLength = recordLength;
        this.keyLength = keyLength;
    }

    @Override
    public String open() {
        if (!Files.isRegularFile(path)) {
            return FileStatus.FILE_NOT_FOUND;
        }
        try {
            List<byte[]> loaded = new ArrayList<>();
            for (String line : Files.readAllLines(path, RecordText.RECORD_CHARSET)) {
                loaded.add(RecordText.toBytes(line));
            }
            loaded.sort(Comparator.comparing(r -> RecordText.toText(r).substring(0, Math.min(keyLength, r.length))));
            records = loaded;
            next = 0;
            eof = false;
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    @Override
    public ReadResult read() {
        if (records == null) {
            return ReadResult.status(FileStatus.NOT_OPEN_FOR_IO);
        }
        if (eof) {
            return ReadResult.status(FileStatus.READ_AFTER_EOF);
        }
        if (next >= records.size()) {
            eof = true;
            return ReadResult.status(FileStatus.END_OF_FILE);
        }
        byte[] line = records.get(next++);
        if (line.length > recordLength) {
            return ReadResult.status(FileStatus.RECORD_LENGTH_MISMATCH);
        }
        byte[] rec = new byte[recordLength];
        java.util.Arrays.fill(rec, (byte) ' ');
        System.arraycopy(line, 0, rec, 0, line.length);
        return ReadResult.ok(rec);
    }

    @Override
    public String close() {
        if (records == null) {
            return FileStatus.NOT_OPEN_FOR_CLOSE;
        }
        records = null;
        return FileStatus.OK;
    }
}
