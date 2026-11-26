package com.carddemo.programs.cbact01c.io;

import java.util.ArrayList;
import java.util.List;

/** In-memory output file for tests; captures written records and can force open/write statuses. */
public final class InMemoryRecordWriter implements RecordWriter {
    private final List<byte[]> records = new ArrayList<>();
    private String openStatus = FileStatus.OK;
    private int failOnWrite = -1;
    private String failStatus;
    private int writes;
    private boolean closed;

    public InMemoryRecordWriter failOpenWith(String status) {
        this.openStatus = status;
        return this;
    }

    /** Forces the {@code writeNumber}-th WRITE (1-based) to return {@code status}; the record is not stored. */
    public InMemoryRecordWriter failWriteWith(int writeNumber, String status) {
        this.failOnWrite = writeNumber;
        this.failStatus = status;
        return this;
    }

    public List<byte[]> records() {
        return records;
    }

    public boolean isClosed() {
        return closed;
    }

    @Override
    public String open() {
        return openStatus;
    }

    @Override
    public String write(byte[] record) {
        writes++;
        if (writes == failOnWrite) {
            return failStatus;
        }
        records.add(record.clone());
        return FileStatus.OK;
    }

    @Override
    public String close() {
        closed = true;
        return FileStatus.OK;
    }
}
