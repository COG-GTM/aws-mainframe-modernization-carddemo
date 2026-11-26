package com.carddemo.programs.corpt00c;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/** In-memory TD queue (the JOBS extrapartition queue feeds the internal reader on the mainframe). */
public final class InMemoryTransientDataQueue implements TransientDataQueue {

    private final Map<String, List<String>> queues = new LinkedHashMap<>();
    private int failOnWrite = -1;
    private CicsResponse failure = new CicsResponse(CicsResponse.QIDERR, 1);
    private int writes;

    /** Make the n-th write (1-based, counted across all queues) and every later one fail with the given RESP/RESP2. */
    public InMemoryTransientDataQueue failFromWrite(int n, CicsResponse response) {
        this.failOnWrite = n;
        this.failure = response;
        return this;
    }

    @Override
    public CicsResponse writeq(String queue, String record) {
        writes++;
        if (failOnWrite > 0 && writes >= failOnWrite) {
            return failure;
        }
        queues.computeIfAbsent(queue, q -> new ArrayList<>()).add(record);
        return CicsResponse.normal();
    }

    public List<String> records(String queue) {
        return List.copyOf(queues.getOrDefault(queue, List.of()));
    }
}
