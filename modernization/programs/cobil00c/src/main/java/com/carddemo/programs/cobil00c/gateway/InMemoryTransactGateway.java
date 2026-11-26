package com.carddemo.programs.cobil00c.gateway;

import com.carddemo.programs.cobil00c.copybook.Cobol;
import com.carddemo.programs.cobil00c.copybook.TranRecord;
import java.util.Collection;
import java.util.EnumMap;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Optional;
import java.util.TreeMap;

/**
 * In-memory TRANSACT KSDS with CICS browse semantics: STARTBR is GTEQ, and a RIDFLD of all X'FF'
 * positions past the last record so the first READPREV returns the highest key; READPREV past the
 * first record raises ENDFILE; ENDBR with no active browse abends (INVREQ, no RESP coded).
 */
public class InMemoryTransactGateway implements TransactGateway {

    public enum Op { STARTBR, READPREV, WRITE }

    private final TreeMap<String, TranRecord> records = new TreeMap<>();
    private final Map<Op, CicsResp> forced = new EnumMap<>(Op.class);
    private boolean browsing;
    private String nextPrevKey;      // key to return on next READPREV (inclusive); null = end of file
    private boolean atStart;

    public InMemoryTransactGateway(Collection<TranRecord> initial) {
        initial.forEach(r -> records.put(r.tranId(), r));
    }

    public void force(Op op, CicsResp resp) {
        forced.put(op, resp);
    }

    public Optional<TranRecord> find(String tranId) {
        return Optional.ofNullable(records.get(tranId));
    }

    public NavigableMap<String, TranRecord> records() {
        return records;
    }

    public boolean isBrowsing() {
        return browsing;
    }

    @Override
    public CicsResponse<Void> startBrowse(String tranIdKey) {
        CicsResp f = forced.get(Op.STARTBR);
        if (f != null) {
            return CicsResponse.of(f, 0);
        }
        atStart = false;
        if (tranIdKey.chars().allMatch(c -> c == Cobol.HIGH_VALUE)) {
            browsing = true;
            nextPrevKey = records.isEmpty() ? null : records.lastKey();
            atStart = records.isEmpty();
            return CicsResponse.normal();
        }
        String k = records.ceilingKey(tranIdKey);
        if (k == null) {
            return CicsResponse.of(CicsResp.NOTFND, 0);
        }
        browsing = true;
        nextPrevKey = k;
        return CicsResponse.normal();
    }

    @Override
    public CicsResponse<TranRecord> readPrev() {
        CicsResp f = forced.get(Op.READPREV);
        if (f != null) {
            return CicsResponse.of(f, 0);
        }
        if (!browsing) {
            return CicsResponse.of(CicsResp.INVREQ, 0);
        }
        if (atStart || nextPrevKey == null) {
            atStart = true;
            return CicsResponse.of(CicsResp.ENDFILE, 0);
        }
        TranRecord r = records.get(nextPrevKey);
        String lower = records.lowerKey(nextPrevKey);
        if (lower == null) {
            atStart = true;
        } else {
            nextPrevKey = lower;
        }
        return CicsResponse.normal(r);
    }

    @Override
    public void endBrowse() {
        if (!browsing) {
            throw new CicsAbendException("AEIP", "INVREQ on ENDBR DATASET(TRANSACT): no browse active");
        }
        browsing = false;
    }

    @Override
    public CicsResponse<Void> write(TranRecord record) {
        CicsResp f = forced.get(Op.WRITE);
        if (f != null) {
            return CicsResponse.of(f, 0);
        }
        if (records.containsKey(record.tranId())) {
            return CicsResponse.of(CicsResp.DUPREC, 0);
        }
        records.put(record.tranId(), record);
        return CicsResponse.normal();
    }
}
