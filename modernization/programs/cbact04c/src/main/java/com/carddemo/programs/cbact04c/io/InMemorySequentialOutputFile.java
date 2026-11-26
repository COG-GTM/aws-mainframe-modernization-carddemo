package com.carddemo.programs.cbact04c.io;

import java.util.ArrayList;
import java.util.List;

/** In-memory sequential output that captures written records. */
public class InMemorySequentialOutputFile<T> implements SequentialOutputFile<T> {

    private final List<T> written = new ArrayList<>();
    private boolean open;

    @Override
    public String open() {
        open = true;
        written.clear();
        return FileStatus.OK;
    }

    @Override
    public String write(T record) {
        if (!open) {
            return FileStatus.WRITE_NOT_OPEN;
        }
        written.add(record);
        return FileStatus.OK;
    }

    @Override
    public String close() {
        if (!open) {
            return FileStatus.CLOSE_NOT_OPEN;
        }
        open = false;
        return FileStatus.OK;
    }

    public List<T> written() {
        return List.copyOf(written);
    }
}
