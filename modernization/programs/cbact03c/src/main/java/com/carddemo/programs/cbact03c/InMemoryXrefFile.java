package com.carddemo.programs.cbact03c;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.List;

/** In-memory XREFFILE; results are replayed in order, then EOF ("10"). Statuses can be scripted. */
public class InMemoryXrefFile implements XrefFile {

    private final Deque<ReadResult> reads;
    private String openStatus = FileStatus.OK;
    private String closeStatus = FileStatus.OK;
    private boolean open;
    private int closeCount;

    public InMemoryXrefFile(List<ReadResult> reads) {
        this.reads = new ArrayDeque<>(reads);
    }

    public static InMemoryXrefFile ofRecords(List<String> records) {
        return new InMemoryXrefFile(records.stream().map(ReadResult::of).toList());
    }

    public InMemoryXrefFile withOpenStatus(String status) {
        this.openStatus = status;
        return this;
    }

    public InMemoryXrefFile withCloseStatus(String status) {
        this.closeStatus = status;
        return this;
    }

    @Override
    public String open() {
        open = FileStatus.OK.equals(openStatus);
        return openStatus;
    }

    @Override
    public ReadResult readNext() {
        if (!open) {
            return ReadResult.status(FileStatus.READ_NOT_OPEN);
        }
        ReadResult next = reads.poll();
        return next == null ? ReadResult.status(FileStatus.END_OF_FILE) : next;
    }

    @Override
    public String close() {
        closeCount++;
        open = false;
        return closeStatus;
    }

    public int closeCount() {
        return closeCount;
    }
}
