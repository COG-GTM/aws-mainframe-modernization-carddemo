package com.carddemo.modernization.batch;

import com.carddemo.modernization.records.codec.RecordCodec;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.TreeMap;

/**
 * In-memory stand-in for a KSDS whose primary key is the leading {@code keyLength} bytes of each record. Keys are
 * held as ISO-8859-1 strings of the raw key bytes, so map order is unsigned byte order, the order the golden run
 * writes indexed files back in.
 */
final class KeyedDataset<T> {
    private final Path file;
    private final RecordCodec<T> codec;
    private final int keyLength;
    private final Charset charset;
    private final TreeMap<String, byte[]> records = new TreeMap<>();
    private boolean dirty;

    private KeyedDataset(Path file, RecordCodec<T> codec, int keyLength, Charset charset) {
        this.file = file;
        this.codec = codec;
        this.keyLength = keyLength;
        this.charset = charset;
    }

    static <T> KeyedDataset<T> load(Path file, RecordCodec<T> codec, int keyLength, Charset charset) {
        KeyedDataset<T> dataset = new KeyedDataset<>(file, codec, keyLength, charset);
        for (byte[] record : FixedLengthFiles.read(file, codec.length())) {
            if (dataset.records.put(dataset.keyOf(record), record) != null) {
                throw new IllegalStateException(file + " has a duplicate key " + dataset.keyOf(record));
            }
        }
        return dataset;
    }

    /** Empties the dataset, as the COBOL does when it opens an indexed file for OUTPUT. */
    void clear() {
        records.clear();
        dirty = true;
    }

    T find(String... keyParts) {
        byte[] record = records.get(key(keyParts));
        return record == null ? null : codec.decode(record, charset);
    }

    void insert(byte[] record) {
        if (records.putIfAbsent(keyOf(record), record) != null) {
            throw new IllegalStateException(file + " already has key " + keyOf(record));
        }
        dirty = true;
    }

    void insert(T record) {
        insert(codec.encode(record, charset));
    }

    boolean rewrite(T record) {
        byte[] encoded = codec.encode(record, charset);
        if (records.replace(keyOf(encoded), encoded) == null) {
            return false;
        }
        dirty = true;
        return true;
    }

    List<T> all() {
        return records.values().stream().map(record -> codec.decode(record, charset)).toList();
    }

    /**
     * Writes back every changed dataset. All new contents are staged to temporary files before any dataset is
     * replaced, so a write failure (a full disk, say) changes none of them. Staged files that are not committed are
     * removed; datasets already replaced stay replaced.
     */
    static void flush(KeyedDataset<?>... datasets) {
        List<KeyedDataset<?>> changed = Arrays.stream(datasets).filter(d -> d.dirty).toList();
        List<Path> staged = new ArrayList<>();
        int committed = 0;
        try {
            for (KeyedDataset<?> dataset : changed) {
                staged.add(FixedLengthFiles.stage(dataset.file, dataset.records.values()));
            }
            for (; committed < changed.size(); committed++) {
                FixedLengthFiles.commit(staged.get(committed), changed.get(committed).file);
                changed.get(committed).dirty = false;
            }
        } catch (RuntimeException e) {
            for (int i = committed; i < staged.size(); i++) {
                FixedLengthFiles.deleteQuietly(staged.get(i), e);
            }
            throw e;
        }
    }

    private String keyOf(byte[] record) {
        return new String(Arrays.copyOf(record, keyLength), StandardCharsets.ISO_8859_1);
    }

    private String key(String... parts) {
        return new String(String.join("", parts).getBytes(charset), StandardCharsets.ISO_8859_1);
    }
}
