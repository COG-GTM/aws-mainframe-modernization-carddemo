package com.carddemo.programs.cbact02c;

import com.carddemo.programs.cbact02c.io.CardFile;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.List;

/** Test double returning pre-scripted FILE STATUS values. */
final class ScriptedCardFile implements CardFile {

    private final String openStatus;
    private final Deque<ReadResult> reads;
    private final String closeStatus;
    int openCalls;
    int readCalls;
    int closeCalls;

    ScriptedCardFile(String openStatus, List<ReadResult> reads, String closeStatus) {
        this.openStatus = openStatus;
        this.reads = new ArrayDeque<>(reads);
        this.closeStatus = closeStatus;
    }

    @Override
    public String open() {
        openCalls++;
        return openStatus;
    }

    @Override
    public ReadResult read() {
        readCalls++;
        return reads.isEmpty() ? new ReadResult("10", null) : reads.poll();
    }

    @Override
    public String close() {
        closeCalls++;
        return closeStatus;
    }
}
