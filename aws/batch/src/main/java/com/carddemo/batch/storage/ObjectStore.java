package com.carddemo.batch.storage;

import java.util.List;
import java.util.Optional;

/** The S3 exchange area ({@code s3://${S3_BUCKET}/…}, batch.md §1.2); a local directory in local runs/tests. */
public interface ObjectStore {

    byte[] get(String key);

    boolean exists(String key);

    void put(String key, byte[] content, String contentType);

    /** Keys under {@code prefix}, sorted ascending. */
    List<String> list(String prefix);

    /** GDG {@code (0)}: the lexicographically last key under {@code prefix}. */
    default Optional<String> latest(String prefix) {
        List<String> keys = list(prefix);
        return keys.isEmpty() ? Optional.empty() : Optional.of(keys.get(keys.size() - 1));
    }

    /** Human-readable location for logs, e.g. {@code s3://bucket/key}. */
    String uri(String key);
}
