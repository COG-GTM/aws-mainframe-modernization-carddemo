package com.carddemo.batch.harness;

import com.carddemo.batch.load.LoadMode;
import com.carddemo.batch.load.LoadResult;
import com.carddemo.batch.load.VsamDatasetLoader;
import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.file.FileStatus;
import com.carddemo.common.file.FileStatusException;
import java.util.ArrayList;
import java.util.List;

/**
 * A VSAM output dataset kept as its PostgreSQL table: records are buffered and stored at {@code CLOSE} through
 * {@link VsamDatasetLoader} ({@link LoadMode#REPLACE} = {@code DELETE}/{@code DEFINE} + {@code REPRO}, or
 * {@link LoadMode#UPSERT}). A record the copybook mapper refuses fails the close with status 22/44.
 */
public final class TableSink implements RecordSink {

    private final String ddname;
    private final Dataset dataset;
    private final LoadMode mode;
    private final VsamDatasetLoader loader;
    private List<FixedWidthRecord> buffer;
    private long count;

    public TableSink(String ddname, Dataset dataset, LoadMode mode, VsamDatasetLoader loader) {
        this.ddname = ddname;
        this.dataset = dataset;
        this.mode = mode;
        this.loader = loader;
    }

    @Override
    public String ddname() {
        return ddname;
    }

    @Override
    public void open() {
        if (buffer != null) {
            throw new FileStatusException(ddname, "OPEN", FileStatus.ALREADY_OPEN);
        }
        buffer = new ArrayList<>();
        count = 0;
    }

    @Override
    public void write(FixedWidthRecord record) {
        if (buffer == null) {
            throw new FileStatusException(ddname, "WRITE", FileStatus.NOT_OPEN_OUTPUT);
        }
        if (record.layout().length() != dataset.mapper().layout().length()) {
            throw new FileStatusException(ddname, "WRITE", FileStatus.RECORD_LENGTH_ERROR);
        }
        buffer.add(record);
        count++;
    }

    @Override
    public long count() {
        return count;
    }

    @Override
    public boolean isOpen() {
        return buffer != null;
    }

    @Override
    public void close() {
        if (buffer == null) {
            return;
        }
        List<FixedWidthRecord> records = buffer;
        buffer = null;
        LoadResult result = loader.load(dataset, records, mode);
        if (!result.rejects().isEmpty()) {
            LoadResult.Reject first = result.rejects().get(0);
            throw new FileStatusException(ddname, "WRITE", FileStatus.DUPLICATE_KEY,
                    new IllegalArgumentException(dataset + " record " + first.recordNumber() + ": " + first.reason()));
        }
    }
}
