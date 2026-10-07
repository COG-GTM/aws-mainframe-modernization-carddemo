package com.carddemo.batch.harness;

import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.file.FileStatus;
import com.carddemo.common.file.FileStatusException;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;

final class LoadedKsdsInput implements KsdsInput {

    private final String ddname;
    private final Supplier<List<FixedWidthRecord>> loader;
    private Iterator<FixedWidthRecord> records;

    LoadedKsdsInput(String ddname, Supplier<List<FixedWidthRecord>> loader) {
        this.ddname = ddname;
        this.loader = loader;
    }

    @Override
    public String ddname() {
        return ddname;
    }

    @Override
    public void open() {
        records = loader.get().iterator();
    }

    @Override
    public Optional<FixedWidthRecord> readNext() {
        if (records == null) {
            throw new FileStatusException(ddname, "READ", FileStatus.NOT_OPEN_INPUT);
        }
        return records.hasNext() ? Optional.of(records.next()) : Optional.empty();
    }

    @Override
    public void close() {
        if (records == null) {
            throw new FileStatusException(ddname, "CLOSE", FileStatus.NOT_OPEN);
        }
        records = null;
    }
}
