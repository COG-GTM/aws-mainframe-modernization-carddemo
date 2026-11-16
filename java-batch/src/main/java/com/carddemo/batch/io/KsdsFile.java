package com.carddemo.batch.io;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.TreeMap;
import java.util.function.Function;

/**
 * In-memory stand-in for a VSAM KSDS: records held in a sorted map by primary key, loaded from a
 * fixed-width file and written back in key order. Supports the random READ / WRITE / REWRITE the
 * COBOL programs issue, with the same duplicate / not-found semantics.
 */
public final class KsdsFile<K extends Comparable<? super K>, V> {

    private final FixedWidthCodec<V> codec;
    private final Function<V, K> keyOf;
    private final TreeMap<K, V> records = new TreeMap<>();

    public KsdsFile(FixedWidthCodec<V> codec, Function<V, K> keyOf) {
        this.codec = codec;
        this.keyOf = keyOf;
    }

    /** Loads every non-empty line of {@code file}; a duplicate key fails like a VSAM load would. */
    public static <K extends Comparable<? super K>, V> KsdsFile<K, V> load(
            Path file, FixedWidthCodec<V> codec, Function<V, K> keyOf) {
        KsdsFile<K, V> ksds = new KsdsFile<>(codec, keyOf);
        try {
            for (String line : Files.readAllLines(file, StandardCharsets.US_ASCII)) {
                if (!line.isEmpty()) {
                    ksds.write(codec.decode(line));
                }
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        return ksds;
    }

    /** Random READ by key (INVALID KEY maps to an empty result). */
    public Optional<V> read(K key) {
        return Optional.ofNullable(records.get(key));
    }

    /** WRITE of a new record; fails on a duplicate key (VSAM status 22). */
    public void write(V record) {
        K key = keyOf.apply(record);
        if (records.putIfAbsent(key, record) != null) {
            throw new IllegalStateException("duplicate key " + key);
        }
    }

    /** REWRITE of an existing record; fails when the key is absent (VSAM status 23). */
    public void rewrite(V record) {
        K key = keyOf.apply(record);
        if (records.replace(key, record) == null) {
            throw new IllegalStateException("record not found for key " + key);
        }
    }

    public boolean contains(K key) {
        return records.containsKey(key);
    }

    public int size() {
        return records.size();
    }

    /** Records in ascending key order (sequential browse). */
    public Collection<V> values() {
        return Collections.unmodifiableCollection(records.values());
    }

    /** Encoded records in key order. */
    public List<String> toLines() {
        List<String> lines = new ArrayList<>(records.size());
        records.values().forEach(v -> lines.add(codec.encode(v)));
        return lines;
    }

    /** Writes the whole file back in key order, LF terminated. */
    public void save(Path file) {
        try {
            Files.write(file, toLines(), StandardCharsets.US_ASCII);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
