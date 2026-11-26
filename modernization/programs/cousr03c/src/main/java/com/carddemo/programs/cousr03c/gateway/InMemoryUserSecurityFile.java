package com.carddemo.programs.cousr03c.gateway;

import com.carddemo.programs.cousr03c.copybook.SecUserData;

import java.io.ByteArrayOutputStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.TreeMap;

/**
 * In-memory USRSEC KSDS. Mirrors CICS rules relevant to COUSR03C: READ UPDATE holds the record
 * for the task, DELETE without RIDFLD needs that hold (INVREQ otherwise). Responses can be forced
 * to exercise WHEN OTHER branches. One instance models one task's view of the file; the READ UPDATE
 * hold is not shared across concurrent tasks.
 */
public final class InMemoryUserSecurityFile implements UserSecurityFile {

    private final TreeMap<String, byte[]> records = new TreeMap<>();
    private String heldKey;
    private CicsResp forcedReadResp;
    private CicsResp forcedDeleteResp;

    /** Fixed-width reader: loads consecutive 80-byte records (e.g. the USRSEC sample dataset). */
    public static InMemoryUserSecurityFile load(byte[] dataset, Charset charset) {
        if (dataset.length % SecUserData.LENGTH != 0) {
            throw new IllegalArgumentException("USRSEC dataset length " + dataset.length
                    + " is not a multiple of " + SecUserData.LENGTH);
        }
        InMemoryUserSecurityFile f = new InMemoryUserSecurityFile();
        for (int off = 0; off < dataset.length; off += SecUserData.LENGTH) {
            byte[] rec = new byte[SecUserData.LENGTH];
            System.arraycopy(dataset, off, rec, 0, SecUserData.LENGTH);
            f.put(SecUserData.parse(rec, charset));
        }
        return f;
    }

    /** Fixed-width writer: dumps records in KSDS key sequence (unsigned byte order in {@code charset}). */
    public byte[] unload(Charset charset) {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        records.values().stream()
                .map(r -> {
                    SecUserData d = new SecUserData();
                    d.moveFrom(r);
                    return d.format(charset);
                })
                .sorted((a, b) -> Arrays.compareUnsigned(a, 0, SecUserData.KEY_LENGTH, b, 0, SecUserData.KEY_LENGTH))
                .forEach(out::writeBytes);
        return out.toByteArray();
    }

    public void put(SecUserData record) {
        records.put(record.usrId(), record.toBytes());
    }

    public Optional<SecUserData> find(String key) {
        byte[] r = records.get(pad(key));
        if (r == null) {
            return Optional.empty();
        }
        SecUserData d = new SecUserData();
        d.moveFrom(r);
        return Optional.of(d);
    }

    public List<String> keys() {
        return new ArrayList<>(records.keySet());
    }

    public int size() {
        return records.size();
    }

    public void forceNextReadResponse(CicsResp resp) {
        this.forcedReadResp = resp;
    }

    public void forceNextDeleteResponse(CicsResp resp) {
        this.forcedDeleteResp = resp;
    }

    public String heldKey() {
        return heldKey;
    }

    @Override
    public ReadResult readForUpdate(String key) {
        if (forcedReadResp != null) {
            CicsResp r = forcedReadResp;
            forcedReadResp = null;
            return new ReadResult(r, null);
        }
        String k = pad(key);
        byte[] r = records.get(k);
        if (r == null) {
            return new ReadResult(CicsResp.of(CicsResp.NOTFND), null);
        }
        heldKey = k;
        SecUserData d = new SecUserData();
        d.moveFrom(r);
        return new ReadResult(CicsResp.of(CicsResp.NORMAL), d);
    }

    @Override
    public CicsResp deleteHeldRecord() {
        if (forcedDeleteResp != null) {
            CicsResp r = forcedDeleteResp;
            forcedDeleteResp = null;
            return r;
        }
        if (heldKey == null) {
            return CicsResp.of(CicsResp.INVREQ);
        }
        String k = heldKey;
        heldKey = null;
        return records.remove(k) == null ? CicsResp.of(CicsResp.NOTFND) : CicsResp.of(CicsResp.NORMAL);
    }

    @Override
    public void endOfTask() {
        heldKey = null;
    }

    private static String pad(String key) {
        String k = key == null ? "" : key;
        return k.length() >= SecUserData.KEY_LENGTH ? k.substring(0, SecUserData.KEY_LENGTH)
                : k + " ".repeat(SecUserData.KEY_LENGTH - k.length());
    }
}
