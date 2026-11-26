package com.carddemo.programs.cousr01c.gateway;

import com.carddemo.programs.cousr01c.copybook.Pic;
import com.carddemo.programs.cousr01c.copybook.SecUserData;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.TreeMap;

/** In-memory USRSEC KSDS keyed by the 8-byte SEC-USR-ID, in EBCDIC key order like VSAM. Thread-safe. */
public final class InMemoryUserSecRepository implements UserSecRepository {

    private final TreeMap<String, SecUserData> records = new TreeMap<>(SecUserData::compareEbcdicKeys);
    private WriteResult forcedResult;

    /** Loads a fixed-width 80-byte-record EBCDIC (Cp037) USRSEC unload, e.g. AWS.M2.CARDDEMO.USRSEC.PS. */
    public static InMemoryUserSecRepository loadEbcdic(InputStream in) throws IOException {
        byte[] all = in.readAllBytes();
        if (all.length % SecUserData.LENGTH != 0) {
            throw new IOException("USRSEC length " + all.length + " is not a multiple of " + SecUserData.LENGTH);
        }
        InMemoryUserSecRepository repo = new InMemoryUserSecRepository();
        for (int off = 0; off < all.length; off += SecUserData.LENGTH) {
            byte[] rec = new byte[SecUserData.LENGTH];
            System.arraycopy(all, off, rec, 0, SecUserData.LENGTH);
            WriteResult r = repo.write(SecUserData.fromEbcdic(rec));
            if (r.resp() != CicsResp.NORMAL) {
                throw new IOException("Duplicate key in USRSEC load at offset " + off);
            }
        }
        return repo;
    }

    /** Makes every following write return the given RESP without storing (fault injection). */
    public synchronized void forceResult(WriteResult result) {
        this.forcedResult = result;
    }

    @Override
    public synchronized WriteResult write(SecUserData record) {
        if (forcedResult != null) {
            return forcedResult;
        }
        try {
            record.toEbcdic();
        } catch (IllegalArgumentException notRepresentable) {
            return new WriteResult(CicsResp.INVREQ, 0);
        }
        if (records.containsKey(record.secUsrId())) {
            return new WriteResult(CicsResp.DUPREC, 0);
        }
        records.put(record.secUsrId(), record);
        return WriteResult.normal();
    }

    public synchronized Optional<SecUserData> find(String secUsrId) {
        return Optional.ofNullable(records.get(Pic.x(secUsrId, 8)));
    }

    public synchronized List<SecUserData> all() {
        return new ArrayList<>(records.values());
    }

    public synchronized int size() {
        return records.size();
    }
}
