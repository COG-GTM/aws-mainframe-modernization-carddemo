package com.carddemo.programs.cotrn01c;

import com.carddemo.programs.cotrn01c.copybook.Cobol;
import com.carddemo.programs.cotrn01c.copybook.TranRecord;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/** In-memory TRANSACT KSDS for tests and local runs. */
public final class InMemoryTransactFileGateway implements TransactFileGateway {

    private final Map<String, TranRecord> records = new TreeMap<>();
    private final List<String> readKeys = new ArrayList<>();
    private Integer forcedResp;
    private int forcedResp2;

    public InMemoryTransactFileGateway() {
    }

    public InMemoryTransactFileGateway(Collection<TranRecord> initial) {
        initial.forEach(this::write);
    }

    public void write(TranRecord record) {
        records.put(record.tranId(), record);
    }

    /** Makes every subsequent READ fail with the given RESP/RESP2 (e.g. NOTOPEN, DISABLED). */
    public void failWith(int resp, int resp2) {
        forcedResp = resp;
        forcedResp2 = resp2;
    }

    public List<String> readKeys() {
        return Collections.unmodifiableList(readKeys);
    }

    public int size() {
        return records.size();
    }

    @Override
    public ReadResult readForUpdate(String tranId) {
        String key = Cobol.moveAlnum(tranId, TranRecord.TRAN_ID_LENGTH);
        readKeys.add(key);
        if (forcedResp != null) {
            return new ReadResult(forcedResp, forcedResp2, null);
        }
        TranRecord r = records.get(key);
        return r == null ? ReadResult.notFound() : ReadResult.normal(r);
    }
}
