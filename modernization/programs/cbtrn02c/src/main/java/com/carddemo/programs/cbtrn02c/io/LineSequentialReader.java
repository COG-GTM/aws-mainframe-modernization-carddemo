package com.carddemo.programs.cbtrn02c.io;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Iterator;
import java.util.List;

/** Fixed-width sequential input read from a line-per-record file (or an in-memory list). */
public class LineSequentialReader implements SequentialReader {

    private final Path path;
    private final List<String> inMemory;
    private final int recordLength;
    private Iterator<String> it;

    public LineSequentialReader(Path path, int recordLength) {
        this.path = path;
        this.inMemory = null;
        this.recordLength = recordLength;
    }

    public LineSequentialReader(List<String> records, int recordLength) {
        this.path = null;
        this.inMemory = records;
        this.recordLength = recordLength;
    }

    @Override
    public String open() {
        if (it != null) {
            return FileStatus.OPEN_ALREADY;
        }
        if (inMemory != null) {
            it = inMemory.stream().map(r -> LineFiles.fit(r, recordLength)).iterator();
            return FileStatus.OK;
        }
        if (!Files.exists(path)) {
            return FileStatus.FILE_NOT_FOUND;
        }
        it = LineFiles.readRecords(path, recordLength).iterator();
        return FileStatus.OK;
    }

    @Override
    public ReadResult read() {
        if (it == null) {
            return ReadResult.of(FileStatus.READ_NOT_OPEN);
        }
        return it.hasNext() ? new ReadResult(FileStatus.OK, it.next()) : ReadResult.of(FileStatus.END_OF_FILE);
    }

    @Override
    public String close() {
        if (it == null) {
            return FileStatus.CLOSE_NOT_OPEN;
        }
        it = null;
        return FileStatus.OK;
    }
}
