package com.carddemo.programs.cbstm03a.io;

import com.carddemo.programs.cbstm03a.support.Pic;
import java.util.ArrayList;
import java.util.List;

/** Collects written records; for tests and embedding. */
public final class InMemoryRecordWriter implements RecordWriter {

    private final int recordLength;
    private final List<String> records = new ArrayList<>();
    private boolean closed;

    public InMemoryRecordWriter(int recordLength) {
        this.recordLength = recordLength;
    }

    @Override
    public void write(String record) {
        records.add(Pic.x(record, recordLength));
    }

    @Override
    public void close() {
        closed = true;
    }

    public List<String> records() {
        return List.copyOf(records);
    }

    public boolean isClosed() {
        return closed;
    }
}
