package com.carddemo.runtime.vsam;

import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Storage SPI behind a {@link KsdsFile}: an ordered map from key bytes to record bytes, ordered by
 * unsigned byte comparison. Implementations: {@link InMemoryKsdsStore}, {@link JdbcKsdsStore}.
 */
public interface KsdsStore {

    Optional<byte[]> get(byte[] key);

    /** Inserts; returns false (no change) when the key already exists. */
    boolean insert(byte[] key, byte[] record);

    /** Replaces an existing record; returns false when the key does not exist. */
    boolean update(byte[] key, byte[] record);

    boolean delete(byte[] key);

    /** First entry with key >= {@code key}. */
    Optional<Map.Entry<byte[], byte[]>> ceiling(byte[] key);

    /** First entry with key > {@code key}. */
    Optional<Map.Entry<byte[], byte[]>> higher(byte[] key);

    /** Last entry with key < {@code key}. */
    Optional<Map.Entry<byte[], byte[]>> lower(byte[] key);

    Optional<Map.Entry<byte[], byte[]>> last();

    /** All entries in key order. */
    List<Map.Entry<byte[], byte[]>> all();

    long count();

    void clear();
}
