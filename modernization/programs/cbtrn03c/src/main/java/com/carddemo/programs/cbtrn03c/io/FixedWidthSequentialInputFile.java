package com.carddemo.programs.cbtrn03c.io;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/** Sequential input data set of fixed-length records. */
public final class FixedWidthSequentialInputFile implements SequentialInputFile {
    private final Path path;
    private final int lrecl;
    private final RecordFormat format;
    private List<String> records;
    private int next;

    public FixedWidthSequentialInputFile(Path path, int lrecl, RecordFormat format) {
        this.path = path;
        this.lrecl = lrecl;
        this.format = format;
    }

    @Override
    public String open() {
        if (!Files.isRegularFile(path)) {
            return FileStatus.FILE_NOT_FOUND;
        }
        try {
            records = FixedWidthRecords.load(path, lrecl, format);
            next = 0;
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    @Override
    public ReadResult read() {
        if (records == null) {
            return ReadResult.status(FileStatus.NOT_OPEN_FOR_READ);
        }
        if (next >= records.size()) {
            return ReadResult.status(FileStatus.END_OF_FILE);
        }
        return ReadResult.ok(records.get(next++));
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
