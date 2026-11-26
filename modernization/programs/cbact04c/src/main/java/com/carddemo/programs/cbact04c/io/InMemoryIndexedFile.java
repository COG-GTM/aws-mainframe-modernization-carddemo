package com.carddemo.programs.cbact04c.io;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

/**
 * In-memory keyed file; insertion order is kept so the contents can be written back in load order.
 * Duplicate keys (non-unique AIX path) keep the first record, and READ returns status 02 like VSAM.
 */
public class InMemoryIndexedFile<K, T> implements IndexedFile<K, T> {

    private final Function<T, K> keyOf;
    private final Map<K, T> records = new LinkedHashMap<>();
    private final Map<K, Integer> keyCounts = new HashMap<>();
    private OpenMode mode;

    public InMemoryIndexedFile(Function<T, K> keyOf, List<T> initial) {
        this.keyOf = keyOf;
        for (T r : initial) {
            K key = keyOf.apply(r);
            records.putIfAbsent(key, r);
            keyCounts.merge(key, 1, Integer::sum);
        }
    }

    @Override
    public String open(OpenMode mode) {
        this.mode = mode;
        return FileStatus.OK;
    }

    @Override
    public ReadResult<T> read(K key) {
        if (mode == null || mode == OpenMode.OUTPUT) {
            return ReadResult.status(FileStatus.READ_NOT_OPEN);
        }
        T r = records.get(key);
        if (r == null) {
            return ReadResult.status(FileStatus.RECORD_NOT_FOUND);
        }
        return keyCounts.get(key) > 1 ? new ReadResult<>(FileStatus.DUPLICATE_KEY, r) : ReadResult.ok(r);
    }

    @Override
    public String rewrite(T record) {
        if (mode != OpenMode.I_O) {
            return FileStatus.REWRITE_NOT_OPEN;
        }
        K key = keyOf.apply(record);
        if (!records.containsKey(key)) {
            return FileStatus.RECORD_NOT_FOUND;
        }
        records.put(key, record);
        return FileStatus.OK;
    }

    @Override
    public String close() {
        if (mode == null) {
            return FileStatus.CLOSE_NOT_OPEN;
        }
        mode = null;
        return FileStatus.OK;
    }

    public T get(K key) {
        return records.get(key);
    }

    public List<T> records() {
        return new ArrayList<>(records.values());
    }
}
