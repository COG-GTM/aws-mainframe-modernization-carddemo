package com.carddemo.programs.cbtrn03c.io;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/** In-memory sequential input with injectable file statuses (for tests and embedding). */
public final class InMemorySequentialInputFile implements SequentialInputFile {
    private final List<String> records;
    private final Map<Integer, String> readFailures = new HashMap<>();
    private String openStatus = FileStatus.OK;
    private String closeStatus = FileStatus.OK;
    private int next;
    private int reads;

    public InMemorySequentialInputFile(List<String> records) {
        this.records = new ArrayList<>(records);
    }

    public InMemorySequentialInputFile failOpen(String status) {
        openStatus = status;
        return this;
    }

    public InMemorySequentialInputFile failClose(String status) {
        closeStatus = status;
        return this;
    }

    /** Make the n-th READ (1-based) return the given status instead of a record. */
    public InMemorySequentialInputFile failRead(int readNumber, String status) {
        readFailures.put(readNumber, status);
        return this;
    }

    @Override
    public String open() {
        return openStatus;
    }

    @Override
    public ReadResult read() {
        reads++;
        String failure = readFailures.get(reads);
        if (failure != null) {
            return ReadResult.status(failure);
        }
        if (next >= records.size()) {
            return ReadResult.status(FileStatus.END_OF_FILE);
        }
        return ReadResult.ok(records.get(next++));
    }

    @Override
    public String close() {
        return closeStatus;
    }
}
