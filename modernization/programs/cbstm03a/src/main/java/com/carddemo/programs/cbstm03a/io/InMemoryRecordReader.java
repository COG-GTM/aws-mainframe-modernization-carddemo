package com.carddemo.programs.cbstm03a.io;

import com.carddemo.programs.cbstm03a.support.Pic;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;

/** Serves records from a list; for tests and embedding. */
public final class InMemoryRecordReader implements RecordReader {

    private final int recordLength;
    private final Iterator<String> it;

    public InMemoryRecordReader(int recordLength, List<String> records) {
        this.recordLength = recordLength;
        this.it = List.copyOf(records).iterator();
    }

    @Override
    public Optional<String> read() {
        return it.hasNext() ? Optional.of(Pic.x(it.next(), recordLength)) : Optional.empty();
    }

    @Override
    public void close() {
    }
}
