package com.carddemo.programs.paudbunl.io;

import java.util.ArrayList;
import java.util.List;

/** In-memory sequential file with optional injected OPEN/CLOSE statuses for error-path testing. */
public final class InMemoryRecordWriter implements RecordWriter {

    private final int lrecl;
    private final List<byte[]> records = new ArrayList<>();
    private String openStatus = FileStatus.SUCCESS;
    private String closeStatus = FileStatus.SUCCESS;
    private String writeStatus = FileStatus.SUCCESS;
    private boolean open;
    private int openCount;
    private int closeCount;

    public InMemoryRecordWriter(int lrecl) {
        this.lrecl = lrecl;
    }

    public InMemoryRecordWriter failOpenWith(String status) {
        this.openStatus = status;
        return this;
    }

    public InMemoryRecordWriter failWriteWith(String status) {
        this.writeStatus = status;
        return this;
    }

    public InMemoryRecordWriter failCloseWith(String status) {
        this.closeStatus = status;
        return this;
    }

    @Override
    public int recordLength() {
        return lrecl;
    }

    @Override
    public String open() {
        openCount++;
        if (FileStatus.isOk(openStatus)) {
            open = true;
        }
        return openStatus;
    }

    @Override
    public String write(byte[] record) {
        if (!open) {
            return FileStatus.WRITE_NOT_OPEN;
        }
        if (record.length != lrecl) {
            return FileStatus.RECORD_LENGTH_CONFLICT;
        }
        if (!FileStatus.isOk(writeStatus)) {
            return writeStatus;
        }
        records.add(record.clone());
        return FileStatus.SUCCESS;
    }

    @Override
    public String close() {
        closeCount++;
        if (!open) {
            return FileStatus.NOT_OPEN;
        }
        open = false;
        return closeStatus;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    public List<byte[]> records() {
        return List.copyOf(records);
    }

    public int openCount() {
        return openCount;
    }

    public int closeCount() {
        return closeCount;
    }
}
