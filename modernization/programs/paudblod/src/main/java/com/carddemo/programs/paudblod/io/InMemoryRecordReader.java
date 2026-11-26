package com.carddemo.programs.paudblod.io;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.List;

/** In-memory reader with scriptable FILE STATUS results, for tests. */
public final class InMemoryRecordReader implements SequentialRecordReader {

    private final Deque<ReadResult> results;
    private String openStatus = FileStatus.SUCCESS;
    private String closeStatus = FileStatus.SUCCESS;
    private boolean opened;
    private boolean closed;

    private InMemoryRecordReader(List<ReadResult> results) {
        this.results = new ArrayDeque<>(results);
    }

    /** Reader returning each record with status '00', then '10'. */
    public static InMemoryRecordReader ofRecords(List<byte[]> records) {
        return new InMemoryRecordReader(records.stream().map(ReadResult::ok).toList());
    }

    /** Reader returning the given results in order, then '10'. */
    public static InMemoryRecordReader ofResults(List<ReadResult> results) {
        return new InMemoryRecordReader(results);
    }

    public InMemoryRecordReader withOpenStatus(String status) {
        this.openStatus = status;
        return this;
    }

    public InMemoryRecordReader withCloseStatus(String status) {
        this.closeStatus = status;
        return this;
    }

    @Override
    public String open() {
        opened = true;
        return openStatus;
    }

    @Override
    public ReadResult read() {
        return results.isEmpty() ? ReadResult.endOfFile() : results.poll();
    }

    @Override
    public String close() {
        closed = true;
        return closeStatus;
    }

    public boolean isOpened() {
        return opened;
    }

    public boolean isClosed() {
        return closed;
    }
}
