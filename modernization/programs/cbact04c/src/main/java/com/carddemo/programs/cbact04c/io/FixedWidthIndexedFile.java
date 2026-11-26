package com.carddemo.programs.cbact04c.io;

import com.carddemo.programs.cbact04c.copybook.RecordCodec;

import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.function.Function;

/**
 * Keyed file backed by a fixed-width record file: loaded on OPEN, written back on CLOSE when opened I-O.
 * The key function selects the access key (primary key, or an alternate key for an AIX path).
 */
public class FixedWidthIndexedFile<K, T> implements IndexedFile<K, T> {

    private final Path path;
    private final RecordCodec<T> codec;
    private final Function<T, K> keyOf;
    private InMemoryIndexedFile<K, T> delegate;
    private OpenMode mode;

    public FixedWidthIndexedFile(Path path, RecordCodec<T> codec, Function<T, K> keyOf) {
        this.path = path;
        this.codec = codec;
        this.keyOf = keyOf;
    }

    @Override
    public String open(OpenMode mode) {
        if (!Files.isRegularFile(path)) {
            return FileStatus.FILE_NOT_FOUND;
        }
        try {
            delegate = new InMemoryIndexedFile<>(keyOf, FixedWidth.readAll(path, codec));
        } catch (UncheckedIOException | IllegalArgumentException e) {
            return FileStatus.PERMANENT_ERROR;
        }
        this.mode = mode;
        return delegate.open(mode);
    }

    @Override
    public ReadResult<T> read(K key) {
        return delegate == null ? ReadResult.status(FileStatus.READ_NOT_OPEN) : delegate.read(key);
    }

    @Override
    public String rewrite(T record) {
        return delegate == null ? FileStatus.REWRITE_NOT_OPEN : delegate.rewrite(record);
    }

    @Override
    public String close() {
        if (delegate == null) {
            return FileStatus.CLOSE_NOT_OPEN;
        }
        String status = delegate.close();
        if (FileStatus.OK.equals(status) && mode == OpenMode.I_O) {
            try {
                FixedWidth.writeAll(path, codec, delegate.records());
            } catch (UncheckedIOException e) {
                status = FileStatus.PERMANENT_ERROR;
            }
        }
        delegate = null;
        mode = null;
        return status;
    }
}
