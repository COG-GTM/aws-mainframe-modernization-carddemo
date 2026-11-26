package com.carddemo.programs.cbtrn03c.io;

import com.carddemo.programs.cbtrn03c.support.Fixed;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/** In-memory sequential output capturing LRECL-padded records, with injectable statuses. */
public final class InMemorySequentialOutputFile implements SequentialOutputFile {
    private final int lrecl;
    private final List<String> records = new ArrayList<>();
    private String openStatus = FileStatus.OK;
    private String closeStatus = FileStatus.OK;
    private int failWriteNumber = -1;
    private String failWriteStatus;
    private int writes;

    public InMemorySequentialOutputFile(int lrecl) {
        this.lrecl = lrecl;
    }

    public InMemorySequentialOutputFile failOpen(String status) {
        openStatus = status;
        return this;
    }

    public InMemorySequentialOutputFile failClose(String status) {
        closeStatus = status;
        return this;
    }

    /** Make the n-th WRITE (1-based) fail with the given status. */
    public InMemorySequentialOutputFile failWrite(int writeNumber, String status) {
        failWriteNumber = writeNumber;
        failWriteStatus = status;
        return this;
    }

    @Override
    public String open() {
        return openStatus;
    }

    @Override
    public String write(String record) {
        writes++;
        if (writes == failWriteNumber) {
            return failWriteStatus;
        }
        records.add(Fixed.pic(record, lrecl));
        return FileStatus.OK;
    }

    @Override
    public String close() {
        return closeStatus;
    }

    public List<String> records() {
        return Collections.unmodifiableList(records);
    }
}
