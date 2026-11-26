package com.carddemo.programs.cbtrn03c.io;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Function;

/** In-memory KSDS with injectable file statuses (for tests and embedding). */
public final class InMemoryKeyedInputFile implements KeyedInputFile {
    private final Map<String, String> records;
    private final Map<String, String> readFailures = new HashMap<>();
    private String openStatus = FileStatus.OK;
    private String closeStatus = FileStatus.OK;

    public InMemoryKeyedInputFile(Map<String, String> records) {
        this.records = new LinkedHashMap<>(records);
    }

    public static InMemoryKeyedInputFile of(Iterable<String> records, Function<String, String> keyOf) {
        Map<String, String> map = new LinkedHashMap<>();
        for (String r : records) {
            map.putIfAbsent(keyOf.apply(r), r);
        }
        return new InMemoryKeyedInputFile(map);
    }

    public InMemoryKeyedInputFile failOpen(String status) {
        openStatus = status;
        return this;
    }

    public InMemoryKeyedInputFile failClose(String status) {
        closeStatus = status;
        return this;
    }

    /** Make a READ of the given key return the status instead of the record. */
    public InMemoryKeyedInputFile failRead(String key, String status) {
        readFailures.put(key, status);
        return this;
    }

    @Override
    public String open() {
        return openStatus;
    }

    @Override
    public ReadResult read(String key) {
        String failure = readFailures.get(key);
        if (failure != null) {
            return ReadResult.status(failure);
        }
        String record = records.get(key);
        return record == null ? ReadResult.status(FileStatus.RECORD_NOT_FOUND) : ReadResult.ok(record);
    }

    @Override
    public String close() {
        return closeStatus;
    }
}
