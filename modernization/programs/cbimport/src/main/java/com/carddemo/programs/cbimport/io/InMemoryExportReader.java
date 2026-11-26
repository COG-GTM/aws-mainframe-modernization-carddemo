package com.carddemo.programs.cbimport.io;

import com.carddemo.programs.cbimport.FileStatus;

import java.util.ArrayList;
import java.util.List;

/** In-memory EXPORT-INPUT with optional injected open/read statuses for error-path tests. */
public final class InMemoryExportReader implements ExportRecordReader {

    private final List<byte[]> records;
    private String openStatus = FileStatus.OK;
    private int failAtRead = -1;
    private String failStatus;
    private int next;
    private int reads;
    private boolean closed;

    public InMemoryExportReader(List<byte[]> records) {
        this.records = new ArrayList<>(records);
    }

    public InMemoryExportReader failOpen(String status) {
        this.openStatus = status;
        return this;
    }

    /** The read with 0-based index {@code readIndex} returns {@code status}. */
    public InMemoryExportReader failRead(int readIndex, String status) {
        this.failAtRead = readIndex;
        this.failStatus = status;
        return this;
    }

    public boolean isClosed() {
        return closed;
    }

    @Override
    public String open() {
        return openStatus;
    }

    @Override
    public ReadResult read() {
        int call = reads++;
        if (call == failAtRead) {
            return new ReadResult(failStatus, null);
        }
        if (next >= records.size()) {
            return new ReadResult(FileStatus.EOF, null);
        }
        return new ReadResult(FileStatus.OK, records.get(next++));
    }

    @Override
    public String close() {
        closed = true;
        return FileStatus.OK;
    }
}
