package com.carddemo.programs.copaua0c.gateway.inmemory;

import com.carddemo.programs.copaua0c.gateway.CicsKsdsFile;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

/** Keyed in-memory VSAM file. {@link #failWith} forces a non-NORMAL/NOTFND response. */
public final class InMemoryKsdsFile<T> implements CicsKsdsFile<T> {

    private final Map<String, T> records = new HashMap<>();
    private final Function<T, String> keyOf;
    private Integer forcedResp;
    private int forcedResp2;

    public InMemoryKsdsFile(Function<T, String> keyOf) {
        this.keyOf = keyOf;
    }

    public InMemoryKsdsFile<T> put(T record) {
        records.put(keyOf.apply(record), record);
        return this;
    }

    public void failWith(int resp, int resp2) {
        forcedResp = resp;
        forcedResp2 = resp2;
    }

    public int size() {
        return records.size();
    }

    @Override
    public ReadResult<T> read(String key) {
        if (forcedResp != null) {
            return new ReadResult<>(forcedResp, forcedResp2, null);
        }
        T r = records.get(key);
        return r == null ? new ReadResult<>(RESP_NOTFND, 0, null) : new ReadResult<>(RESP_NORMAL, 0, r);
    }
}
