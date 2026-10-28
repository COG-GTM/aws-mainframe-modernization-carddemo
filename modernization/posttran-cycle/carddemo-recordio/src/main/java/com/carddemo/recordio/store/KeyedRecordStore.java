package com.carddemo.recordio.store;

import com.carddemo.recordio.codec.FixedWidthRecord;
import com.carddemo.recordio.codec.RecordEncoding;
import com.carddemo.recordio.layout.RecordLayout;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.stream.Stream;

/**
 * In-memory image of a VSAM KSDS: records ordered by primary key with READ / WRITE / REWRITE /
 * browse semantics. Loaded from and saved to a fixed-width dataset image.
 *
 * <p>REWRITE re-encodes the changed fields into the record image that was originally read, so
 * FILLER and untouched fields survive byte-for-byte as they do on the mainframe. This is a
 * transition-period substitute for a relational table; see docs/modernization/data-model.md.
 *
 * @param <T> decoded record type
 */
public final class KeyedRecordStore<T> {

    private final String datasetName;
    private final RecordLayout<T> layout;
    private final RecordEncoding encoding;
    private final Function<T, String> primaryKey;
    private final TreeMap<String, FixedWidthRecord> images = new TreeMap<>();

    public KeyedRecordStore(String datasetName, RecordLayout<T> layout, RecordEncoding encoding,
                            Function<T, String> primaryKey) {
        this.datasetName = datasetName;
        this.layout = layout;
        this.encoding = encoding;
        this.primaryKey = primaryKey;
    }

    public static <T> KeyedRecordStore<T> load(String datasetName, Path path, RecordLayout<T> layout,
                                               RecordEncoding encoding, Function<T, String> primaryKey) {
        try {
            return fromBytes(datasetName, Files.readAllBytes(path), layout, encoding, primaryKey);
        } catch (IOException e) {
            throw new UncheckedIOException("cannot read " + path, e);
        }
    }

    public static <T> KeyedRecordStore<T> fromBytes(String datasetName, byte[] bytes, RecordLayout<T> layout,
                                                    RecordEncoding encoding, Function<T, String> primaryKey) {
        KeyedRecordStore<T> store = new KeyedRecordStore<>(datasetName, layout, encoding, primaryKey);
        for (FixedWidthRecord image : FixedWidthFile.split(bytes, layout.length(), encoding)) {
            T value = layout.decode(image);
            String key = primaryKey.apply(value);
            if (store.images.putIfAbsent(key, image) != null) {
                throw new DuplicateKeyException(datasetName, key);
            }
        }
        return store;
    }

    public static <T> KeyedRecordStore<T> of(String datasetName, List<T> values, RecordLayout<T> layout,
                                             RecordEncoding encoding, Function<T, String> primaryKey) {
        KeyedRecordStore<T> store = new KeyedRecordStore<>(datasetName, layout, encoding, primaryKey);
        values.forEach(store::write);
        return store;
    }

    public String datasetName() {
        return datasetName;
    }

    /** READ ... INVALID KEY -> empty. */
    public Optional<T> read(String key) {
        return Optional.ofNullable(images.get(key)).map(layout::decode);
    }

    public boolean contains(String key) {
        return images.containsKey(key);
    }

    /** WRITE: a brand-new record image (spaces in FILLER) keyed by the value's primary key. */
    public void write(T value) {
        String key = primaryKey.apply(value);
        if (images.containsKey(key)) {
            throw new DuplicateKeyException(datasetName, key);
        }
        images.put(key, layout.encode(value, encoding));
    }

    /** REWRITE: overwrite the named fields of the existing image, keep everything else. */
    public void rewrite(T value) {
        String key = primaryKey.apply(value);
        FixedWidthRecord image = images.get(key);
        if (image == null) {
            throw new RecordNotFoundException(datasetName, key);
        }
        layout.encodeInto(image, value);
    }

    /** START ... KEY >= key, then READ NEXT until end of file. */
    public Stream<T> browseFrom(String key) {
        return images.tailMap(key, true).values().stream().map(layout::decode);
    }

    public Stream<T> readAll() {
        return images.values().stream().map(layout::decode);
    }

    public int size() {
        return images.size();
    }

    public List<FixedWidthRecord> images() {
        return new ArrayList<>(images.values());
    }

    public byte[] toBytes() {
        return FixedWidthFile.join(images());
    }

    public void save(Path path) {
        FixedWidthFile.write(path, images());
    }
}
