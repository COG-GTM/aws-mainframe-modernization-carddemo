package com.carddemo.programs.cbtrn01c.io;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

/** In-memory KSDS with injectable FILE STATUS results for testing error paths. */
public final class InMemoryIndexedFile<R> implements IndexedInputFile<R> {

    private final Map<String, R> records = new HashMap<>();
    private final List<String> keysRead = new ArrayList<>();
    private String openStatus = FileStatus.OK;
    private String closeStatus = FileStatus.OK;
    private String readStatusOverride;
    private boolean opened;
    private boolean closed;

    public InMemoryIndexedFile(List<R> content, Function<R, String> keyOf) {
        for (R r : content) {
            records.putIfAbsent(keyOf.apply(r), r);
        }
    }

    public static <R> InMemoryIndexedFile<R> empty() {
        return new InMemoryIndexedFile<>(List.of(), r -> "");
    }

    public InMemoryIndexedFile<R> withOpenStatus(String status) {
        this.openStatus = status;
        return this;
    }

    public InMemoryIndexedFile<R> withCloseStatus(String status) {
        this.closeStatus = status;
        return this;
    }

    /** Every READ returns {@code status} (e.g. a non-INVALID-KEY I/O error). */
    public InMemoryIndexedFile<R> withReadStatus(String status) {
        this.readStatusOverride = status;
        return this;
    }

    @Override
    public String open() {
        opened = FileStatus.OK.equals(openStatus);
        return openStatus;
    }

    @Override
    public ReadResult<R> read(String key) {
        keysRead.add(key);
        if (!opened) {
            return ReadResult.of(FileStatus.READ_NOT_OPEN);
        }
        if (readStatusOverride != null) {
            return ReadResult.of(readStatusOverride);
        }
        R r = records.get(key);
        return r == null ? ReadResult.of(FileStatus.RECORD_NOT_FOUND) : new ReadResult<>(FileStatus.OK, r);
    }

    @Override
    public String close() {
        closed = true;
        return closeStatus;
    }

    public List<String> keysRead() {
        return List.copyOf(keysRead);
    }

    public boolean isClosed() {
        return closed;
    }
}
