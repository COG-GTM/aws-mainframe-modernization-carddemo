package com.carddemo.programs.cbact01c.io;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

/**
 * In-memory input file for tests. Returns the given records in order, then status 10. Statuses for open,
 * close, or the n-th read can be forced to exercise error paths.
 */
public final class InMemoryRecordReader implements SequentialRecordReader {
    private final Deque<byte[]> records;
    private String openStatus = FileStatus.OK;
    private String closeStatus = FileStatus.OK;
    private int failOnRead = -1;
    private String failStatus;
    private int reads;
    private boolean closed;

    public InMemoryRecordReader(List<byte[]> records) {
        this.records = new ArrayDeque<>(new ArrayList<>(records));
    }

    public InMemoryRecordReader failOpenWith(String status) {
        this.openStatus = status;
        return this;
    }

    public InMemoryRecordReader failCloseWith(String status) {
        this.closeStatus = status;
        return this;
    }

    /** Forces the {@code readNumber}-th READ (1-based) to return {@code status}. */
    public InMemoryRecordReader failReadWith(int readNumber, String status) {
        this.failOnRead = readNumber;
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
        reads++;
        if (reads == failOnRead) {
            return ReadResult.status(failStatus);
        }
        byte[] next = records.poll();
        return next == null ? ReadResult.status(FileStatus.END_OF_FILE) : ReadResult.ok(next);
    }

    @Override
    public String close() {
        closed = true;
        return closeStatus;
    }
}
