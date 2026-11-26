package com.carddemo.programs.cbact04c.io;

import java.util.ArrayList;
import java.util.List;

/** In-memory sequential input; records are returned in list order. */
public class InMemorySequentialInputFile<T> implements SequentialInputFile<T> {

    private final List<T> records;
    private boolean open;
    private int next;

    public InMemorySequentialInputFile(List<T> records) {
        this.records = new ArrayList<>(records);
    }

    @Override
    public String open() {
        open = true;
        next = 0;
        return FileStatus.OK;
    }

    @Override
    public ReadResult<T> read() {
        if (!open) {
            return ReadResult.status(FileStatus.READ_NOT_OPEN);
        }
        if (next >= records.size()) {
            return ReadResult.status(FileStatus.END_OF_FILE);
        }
        return ReadResult.ok(records.get(next++));
    }

    @Override
    public String close() {
        if (!open) {
            return FileStatus.CLOSE_NOT_OPEN;
        }
        open = false;
        return FileStatus.OK;
    }
}
