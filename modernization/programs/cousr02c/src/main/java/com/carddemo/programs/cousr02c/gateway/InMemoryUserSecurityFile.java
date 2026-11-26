package com.carddemo.programs.cousr02c.gateway;

import com.carddemo.programs.cousr02c.copybook.CicsResp;
import com.carddemo.programs.cousr02c.copybook.Pic;
import com.carddemo.programs.cousr02c.copybook.SecUserData;

import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Deque;
import java.util.List;
import java.util.Optional;
import java.util.TreeMap;

/**
 * In-memory KSDS emulation of USRSEC for tests. Keys are exact 8-byte matches (no case folding). REWRITE is only
 * accepted after a READ UPDATE of the same key in the same task, as in CICS; otherwise INVREQ is returned.
 * Responses can be injected to exercise the non-NORMAL branches.
 */
public final class InMemoryUserSecurityFile implements UserSecurityFile {

    public static final String DATASET = "USRSEC";

    private final TreeMap<String, String> records = new TreeMap<>();
    private final Deque<CicsResult> injectedReads = new ArrayDeque<>();
    private final Deque<CicsResult> injectedRewrites = new ArrayDeque<>();
    private String heldKey;
    private int rewriteCount;

    public InMemoryUserSecurityFile() {
    }

    public InMemoryUserSecurityFile(Collection<SecUserData> initial) {
        initial.forEach(this::put);
    }

    public void put(SecUserData record) {
        records.put(record.getSecUsrId(), record.toRecord());
    }

    public Optional<SecUserData> find(String key) {
        return Optional.ofNullable(records.get(Pic.x(key, SecUserData.USR_ID_LEN))).map(SecUserData::fromRecord);
    }

    public List<SecUserData> all() {
        return records.values().stream().map(SecUserData::fromRecord).toList();
    }

    public int rewriteCount() {
        return rewriteCount;
    }

    public void injectNextRead(int resp, int resp2) {
        injectedReads.add(new CicsResult(resp, resp2));
    }

    public void injectNextRewrite(int resp, int resp2) {
        injectedRewrites.add(new CicsResult(resp, resp2));
    }

    private static boolean known(String dataset) {
        return dataset != null && Pic.trimRight(dataset).equals(DATASET);
    }

    @Override
    public ReadResult readForUpdate(String dataset, String key) {
        if (!injectedReads.isEmpty()) {
            return new ReadResult(injectedReads.poll(), null);
        }
        if (!known(dataset)) {
            return new ReadResult(new CicsResult(CicsResp.FILENOTFOUND, 1), null);
        }
        String rec = records.get(key);
        if (rec == null) {
            return new ReadResult(new CicsResult(CicsResp.NOTFND, 80), null);
        }
        heldKey = key;
        return new ReadResult(CicsResult.NORMAL, SecUserData.fromRecord(rec));
    }

    @Override
    public CicsResult rewrite(String dataset, SecUserData record) {
        if (!injectedRewrites.isEmpty()) {
            heldKey = null;
            return injectedRewrites.poll();
        }
        if (!known(dataset)) {
            return new CicsResult(CicsResp.FILENOTFOUND, 1);
        }
        if (heldKey == null || !heldKey.equals(record.getSecUsrId())) {
            return new CicsResult(CicsResp.INVREQ, 30);
        }
        records.put(heldKey, record.toRecord());
        heldKey = null;
        rewriteCount++;
        return CicsResult.NORMAL;
    }

    @Override
    public void endTask() {
        heldKey = null;
    }
}
