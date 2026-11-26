package com.carddemo.programs.cbimport.io;

import com.carddemo.programs.cbimport.FileStatus;

import java.util.ArrayList;
import java.util.List;

/** In-memory output file with optional injected open/write statuses for error-path tests. */
public final class InMemoryRecordWriter implements RecordWriter {

    private final List<String> records = new ArrayList<>();
    private String openStatus = FileStatus.OK;
    private String writeStatus = FileStatus.OK;
    private boolean closed;

    public InMemoryRecordWriter failOpen(String status) {
        this.openStatus = status;
        return this;
    }

    public InMemoryRecordWriter failWrite(String status) {
        this.writeStatus = status;
        return this;
    }

    public List<String> records() {
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
    public String write(String record) {
        if (FileStatus.isOk(writeStatus)) {
            records.add(record);
        }
        return writeStatus;
    }

    @Override
    public String close() {
        closed = true;
        return FileStatus.OK;
    }
}
