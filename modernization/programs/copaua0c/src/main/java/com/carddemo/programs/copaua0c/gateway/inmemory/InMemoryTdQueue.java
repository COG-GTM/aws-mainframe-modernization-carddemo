package com.carddemo.programs.copaua0c.gateway.inmemory;

import com.carddemo.programs.copaua0c.copybook.ErrorLogRecord;
import com.carddemo.programs.copaua0c.gateway.TransientDataQueue;
import java.util.ArrayList;
import java.util.List;

public final class InMemoryTdQueue implements TransientDataQueue {

    public record Entry(String queue, ErrorLogRecord record) {
    }

    private final List<Entry> entries = new ArrayList<>();

    public List<Entry> entries() {
        return entries;
    }

    @Override
    public void writeq(String queue, ErrorLogRecord record) {
        entries.add(new Entry(queue, record.copy()));
    }
}
