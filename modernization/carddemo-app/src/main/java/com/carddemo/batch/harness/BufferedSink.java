package com.carddemo.batch.harness;

import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.file.FileStatus;
import com.carddemo.common.file.FileStatusException;
import java.util.ArrayList;
import java.util.List;

/** An output dataset kept in memory until the step ends, then catalogued as a dated generation (ADR-0012). */
public final class BufferedSink implements RecordSink {

    private final String ddname;
    private final List<FixedWidthRecord> records = new ArrayList<>();
    private boolean open;

    public BufferedSink(String ddname) {
        this.ddname = ddname;
    }

    @Override
    public String ddname() {
        return ddname;
    }

    @Override
    public void open() {
        if (open) {
            throw new FileStatusException(ddname, "OPEN", FileStatus.ALREADY_OPEN);
        }
        records.clear();
        open = true;
    }

    @Override
    public void write(FixedWidthRecord record) {
        if (!open) {
            throw new FileStatusException(ddname, "WRITE", FileStatus.NOT_OPEN_OUTPUT);
        }
        records.add(record);
    }

    @Override
    public long count() {
        return records.size();
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public void close() {
        open = false;
    }

    public List<FixedWidthRecord> records() {
        return List.copyOf(records);
    }
}
