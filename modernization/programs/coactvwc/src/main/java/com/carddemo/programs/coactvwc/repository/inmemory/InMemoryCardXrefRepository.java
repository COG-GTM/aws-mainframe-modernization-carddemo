package com.carddemo.programs.coactvwc.repository.inmemory;

import com.carddemo.programs.coactvwc.copybook.CardXrefRecord;
import com.carddemo.programs.coactvwc.repository.CicsResp;
import com.carddemo.programs.coactvwc.repository.CardXrefRepository;
import com.carddemo.programs.coactvwc.repository.FileReadResult;
import java.util.Collection;
import java.util.List;
import java.util.TreeMap;

/**
 * In-memory CARDXREF KSDS with the CXACAIX path. A non-unique AIX key returns the first record in primary-key
 * order with RESP DUPKEY, as CICS does for a path READ when more records share the key.
 */
public final class InMemoryCardXrefRepository implements CardXrefRepository {
    private final TreeMap<String, CardXrefRecord> byCardNum = new TreeMap<>();

    public InMemoryCardXrefRepository(Collection<CardXrefRecord> records) {
        records.forEach(this::put);
    }

    /** CICS READ DUPKEY RESP2: another record with the same non-unique alternate-index key follows. */
    static final int DUPKEY_RESP2_NONUNIQUE_AIX = 140;

    public void put(CardXrefRecord r) {
        byCardNum.put(r.xrefCardNum(), r);
    }

    @Override
    public FileReadResult<CardXrefRecord> readByAcctId(String acctIdX) {
        List<CardXrefRecord> hits = byCardNum.values().stream().filter(r -> r.acctIdKey().equals(acctIdX)).toList();
        if (hits.isEmpty()) {
            return FileReadResult.notFound();
        }
        if (hits.size() > 1) {
            return new FileReadResult<>(CicsResp.DUPKEY, DUPKEY_RESP2_NONUNIQUE_AIX, hits.get(0));
        }
        return FileReadResult.normal(hits.get(0));
    }
}
