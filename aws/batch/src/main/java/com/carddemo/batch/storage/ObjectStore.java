package com.carddemo.batch.storage;

import java.io.InputStream;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;

/** The S3 exchange area ({@code s3://${S3_BUCKET}/…}, batch.md §1.2); a local directory in local runs/tests. */
public interface ObjectStore {

    byte[] get(String key);

    InputStream open(String key);

    boolean exists(String key);

    void put(String key, byte[] content, String contentType);

    void put(String key, Path file, String contentType);

    /** Keys under {@code prefix}, sorted ascending. */
    List<String> list(String prefix);

    /** GDG {@code (0)}: the most recently written key under {@code prefix} (ties: the greatest key). */
    Optional<String> latest(String prefix);

    /** Human-readable location for logs, e.g. {@code s3://bucket/key}. */
    String uri(String key);
}
