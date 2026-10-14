package com.carddemo.runtime.vsam;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;

/** {@link KsdsStore} backed by a TreeMap (unsigned byte order). */
public final class InMemoryKsdsStore implements KsdsStore {

    private final TreeMap<byte[], byte[]> map = new TreeMap<>(Arrays::compareUnsigned);

    @Override
    public synchronized Optional<byte[]> get(byte[] key) {
        return Optional.ofNullable(map.get(key)).map(byte[]::clone);
    }

    @Override
    public synchronized boolean insert(byte[] key, byte[] record) {
        if (map.containsKey(key)) {
            return false;
        }
        map.put(key.clone(), record.clone());
        return true;
    }

    @Override
    public synchronized boolean update(byte[] key, byte[] record) {
        if (!map.containsKey(key)) {
            return false;
        }
        map.put(key.clone(), record.clone());
        return true;
    }

    @Override
    public synchronized boolean delete(byte[] key) {
        return map.remove(key) != null;
    }

    @Override
    public synchronized Optional<Map.Entry<byte[], byte[]>> ceiling(byte[] key) {
        return copy(map.ceilingEntry(key));
    }

    @Override
    public synchronized Optional<Map.Entry<byte[], byte[]>> higher(byte[] key) {
        return copy(map.higherEntry(key));
    }

    @Override
    public synchronized Optional<Map.Entry<byte[], byte[]>> lower(byte[] key) {
        return copy(map.lowerEntry(key));
    }

    @Override
    public synchronized Optional<Map.Entry<byte[], byte[]>> last() {
        return copy(map.isEmpty() ? null : map.lastEntry());
    }

    @Override
    public synchronized List<Map.Entry<byte[], byte[]>> all() {
        List<Map.Entry<byte[], byte[]>> out = new ArrayList<>(map.size());
        map.forEach((k, v) -> out.add(Map.entry(k.clone(), v.clone())));
        return out;
    }

    @Override
    public synchronized long count() {
        return map.size();
    }

    @Override
    public synchronized void clear() {
        map.clear();
    }

    private static Optional<Map.Entry<byte[], byte[]>> copy(Map.Entry<byte[], byte[]> e) {
        return e == null ? Optional.empty() : Optional.of(Map.entry(e.getKey().clone(), e.getValue().clone()));
    }
}
