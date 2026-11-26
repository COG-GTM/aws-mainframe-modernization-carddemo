package com.carddemo.programs.cbact04c.io;

import com.carddemo.programs.cbact04c.copybook.RecordCodec;

import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/** Sequential input over a fixed-width record file. OPEN of a missing file returns status '35'. */
public class FixedWidthSequentialInputFile<T> implements SequentialInputFile<T> {

    private final Path path;
    private final RecordCodec<T> codec;
    private InMemorySequentialInputFile<T> delegate;

    public FixedWidthSequentialInputFile(Path path, RecordCodec<T> codec) {
        this.path = path;
        this.codec = codec;
    }

    @Override
    public String open() {
        if (!Files.isRegularFile(path)) {
            return FileStatus.FILE_NOT_FOUND;
        }
        try {
            List<T> records = FixedWidth.readAll(path, codec);
            delegate = new InMemorySequentialInputFile<>(records);
        } catch (UncheckedIOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
        return delegate.open();
    }

    @Override
    public ReadResult<T> read() {
        return delegate == null ? ReadResult.status(FileStatus.READ_NOT_OPEN) : delegate.read();
    }

    @Override
    public String close() {
        if (delegate == null) {
            return FileStatus.CLOSE_NOT_OPEN;
        }
        String status = delegate.close();
        delegate = null;
        return status;
    }
}
