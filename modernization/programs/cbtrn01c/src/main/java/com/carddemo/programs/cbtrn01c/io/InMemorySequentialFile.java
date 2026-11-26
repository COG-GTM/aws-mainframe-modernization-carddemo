package com.carddemo.programs.cbtrn01c.io;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/** In-memory sequential file with injectable FILE STATUS results for testing error paths. */
public final class InMemorySequentialFile<R> implements SequentialInputFile<R> {

    private final List<R> records;
    private final Map<Integer, String> readFailures = new HashMap<>();
    private String openStatus = FileStatus.OK;
    private String closeStatus = FileStatus.OK;
    private int next;
    private boolean opened;
    private boolean closed;

    public InMemorySequentialFile(List<R> records) {
        this.records = new ArrayList<>(records);
    }

    public InMemorySequentialFile<R> withOpenStatus(String status) {
        this.openStatus = status;
        return this;
    }

    public InMemorySequentialFile<R> withCloseStatus(String status) {
        this.closeStatus = status;
        return this;
    }

    /** The {@code readIndex}-th READ (0-based) returns {@code status} instead of a record. */
    public InMemorySequentialFile<R> failReadAt(int readIndex, String status) {
        readFailures.put(readIndex, status);
        return this;
    }

    @Override
    public String open() {
        opened = FileStatus.OK.equals(openStatus);
        return openStatus;
    }

    @Override
    public ReadResult<R> read() {
        if (!opened) {
            return ReadResult.of(FileStatus.READ_NOT_OPEN);
        }
        int attempt = next++;
        String failure = readFailures.get(attempt);
        if (failure != null) {
            return ReadResult.of(failure);
        }
        int recordIndex = (int) (attempt - readFailures.keySet().stream().filter(i -> i < attempt).count());
        if (recordIndex >= records.size()) {
            return ReadResult.of(FileStatus.END_OF_FILE);
        }
        return new ReadResult<>(FileStatus.OK, records.get(recordIndex));
    }

    @Override
    public String close() {
        closed = true;
        return closeStatus;
    }

    public boolean isClosed() {
        return closed;
    }
}
