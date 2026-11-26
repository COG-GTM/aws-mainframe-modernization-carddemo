package com.carddemo.programs.cbtrn02c.io;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/** Fixed-width sequential output; records are kept in memory and, if a path is given, written on CLOSE. */
public class LineSequentialWriter implements SequentialWriter {

    private final Path path;
    private final int recordLength;
    private final List<String> written = new ArrayList<>();
    private boolean open;

    public LineSequentialWriter(Path path, int recordLength) {
        this.path = path;
        this.recordLength = recordLength;
    }

    public LineSequentialWriter(int recordLength) {
        this(null, recordLength);
    }

    public List<String> records() {
        return List.copyOf(written);
    }

    @Override
    public String open() {
        if (open) {
            return FileStatus.OPEN_ALREADY;
        }
        written.clear();
        open = true;
        return FileStatus.OK;
    }

    @Override
    public String write(String record) {
        if (!open) {
            return FileStatus.WRITE_NOT_OPEN;
        }
        written.add(LineFiles.fit(record, recordLength));
        return FileStatus.OK;
    }

    @Override
    public String close() {
        if (!open) {
            return FileStatus.CLOSE_NOT_OPEN;
        }
        open = false;
        if (path != null) {
            try {
                LineFiles.writeRecords(path, written);
            } catch (RuntimeException e) {
                return FileStatus.PERMANENT_ERROR;
            }
        }
        return FileStatus.OK;
    }
}
