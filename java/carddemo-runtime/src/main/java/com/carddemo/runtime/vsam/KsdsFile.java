package com.carddemo.runtime.vsam;

import com.carddemo.runtime.cics.CicsResp;
import com.carddemo.runtime.cobol.PicX;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * A VSAM KSDS as seen through CICS file control (READ, READ UPDATE, WRITE, REWRITE, DELETE, STARTBR,
 * READNEXT, READPREV, RESETBR, ENDBR) with CICS RESP semantics. Keys and records are ISO-8859-1 strings.
 */
public final class KsdsFile {

    private final String name;
    private final int recordLength;
    private final int keyOffset;
    private final int keyLength;
    private final KsdsStore store;
    private FileState state = FileState.ENABLED;
    private byte[] heldForUpdate;
    private final Map<Integer, Browse> browses = new HashMap<>();

    private static final class Browse {
        final byte[] requestedKey;
        byte[] lastKey;
        boolean lastWasNext;

        Browse(byte[] requestedKey) {
            this.requestedKey = requestedKey;
        }
    }

    public KsdsFile(String name, int recordLength, int keyOffset, int keyLength, KsdsStore store) {
        this.name = name.trim();
        this.recordLength = recordLength;
        this.keyOffset = keyOffset;
        this.keyLength = keyLength;
        this.store = store;
    }

    public static KsdsFile inMemory(String name, int recordLength, int keyOffset, int keyLength) {
        return new KsdsFile(name, recordLength, keyOffset, keyLength, new InMemoryKsdsStore());
    }

    public String name() {
        return name;
    }

    public int recordLength() {
        return recordLength;
    }

    public int keyOffset() {
        return keyOffset;
    }

    public int keyLength() {
        return keyLength;
    }

    public KsdsStore store() {
        return store;
    }

    public void setState(FileState newState) {
        this.state = newState;
    }

    /** Loads records (IDCAMS REPRO into an empty cluster). Duplicate keys are rejected. */
    public int load(List<String> records) {
        int n = 0;
        for (String r : records) {
            byte[] rec = PicX.toBytes(PicX.of(r, recordLength));
            if (!store.insert(keyOf(rec), rec)) {
                throw new IllegalArgumentException("Duplicate key loading " + name + ": " + PicX.fromBytes(keyOf(rec)));
            }
            n++;
        }
        return n;
    }

    /** READ with full-key equality. LENGERR when the record is longer than {@code intoLength} (data truncated). */
    public FileResult read(String key, int intoLength) {
        Optional<FileResult> unusable = checkUsable();
        if (unusable.isPresent()) {
            return unusable.get();
        }
        if (key.length() != keyLength) {
            return FileResult.of(CicsResp.INVREQ, 25);
        }
        Optional<byte[]> rec = store.get(PicX.toBytes(key));
        if (rec.isEmpty()) {
            return FileResult.of(CicsResp.NOTFND, 80);
        }
        byte[] data = rec.get();
        if (data.length > intoLength) {
            return new FileResult(CicsResp.LENGERR, 11, Arrays.copyOf(data, intoLength), key, -1);
        }
        return FileResult.record(CicsResp.NORMAL, data, key);
    }

    public FileResult read(String key) {
        return read(key, recordLength);
    }

    /** READ ... GTEQ: first record with key >= {@code key} (shorter keys are treated as generic). */
    public FileResult readGteq(String key) {
        Optional<FileResult> unusable = checkUsable();
        if (unusable.isPresent()) {
            return unusable.get();
        }
        return store.ceiling(PicX.toBytes(key)).map(e -> FileResult.record(CicsResp.NORMAL, e.getValue(), PicX.fromBytes(e.getKey())))
                .orElse(FileResult.of(CicsResp.NOTFND, 80));
    }

    /** READ ... UPDATE: reads and holds the record for REWRITE / DELETE. */
    public FileResult readForUpdate(String key) {
        FileResult r = read(key);
        if (r.isNormal()) {
            heldForUpdate = PicX.toBytes(key);
        }
        return r;
    }

    public FileResult write(String record) {
        Optional<FileResult> unusable = checkUsable();
        if (unusable.isPresent()) {
            return unusable.get();
        }
        if (record.length() != recordLength) {
            return FileResult.of(CicsResp.LENGERR, 12);
        }
        byte[] rec = PicX.toBytes(record);
        byte[] key = keyOf(rec);
        if (!store.insert(key, rec)) {
            return FileResult.of(CicsResp.DUPREC);
        }
        return FileResult.record(CicsResp.NORMAL, rec, PicX.fromBytes(key));
    }

    /** REWRITE: requires a prior READ UPDATE of the same key (INVREQ otherwise). */
    public FileResult rewrite(String record) {
        Optional<FileResult> unusable = checkUsable();
        if (unusable.isPresent()) {
            return unusable.get();
        }
        if (record.length() != recordLength) {
            return FileResult.of(CicsResp.LENGERR, 12);
        }
        byte[] rec = PicX.toBytes(record);
        byte[] key = keyOf(rec);
        if (heldForUpdate == null) {
            return FileResult.of(CicsResp.INVREQ, 30);
        }
        if (!Arrays.equals(heldForUpdate, key)) {
            return FileResult.of(CicsResp.INVREQ, 31);
        }
        heldForUpdate = null;
        store.update(key, rec);
        return FileResult.record(CicsResp.NORMAL, rec, PicX.fromBytes(key));
    }

    /** DELETE without RIDFLD: deletes the record held by READ UPDATE. */
    public FileResult deleteHeld() {
        if (heldForUpdate == null) {
            return FileResult.of(CicsResp.INVREQ, 30);
        }
        byte[] key = heldForUpdate;
        heldForUpdate = null;
        return delete(PicX.fromBytes(key));
    }

    public FileResult delete(String key) {
        Optional<FileResult> unusable = checkUsable();
        if (unusable.isPresent()) {
            return unusable.get();
        }
        return store.delete(PicX.toBytes(key)) ? FileResult.of(CicsResp.NORMAL) : FileResult.of(CicsResp.NOTFND, 80);
    }

    /** UNLOCK: releases a READ UPDATE hold. */
    public FileResult unlock() {
        heldForUpdate = null;
        return FileResult.of(CicsResp.NORMAL);
    }

    /** STARTBR RIDFLD(key) GTEQ (or EQUAL). An all-HIGH-VALUES key positions at end-of-file for READPREV. */
    public FileResult startbr(String key, boolean equal, int reqid) {
        Optional<FileResult> unusable = checkUsable();
        if (unusable.isPresent()) {
            return unusable.get();
        }
        byte[] k = PicX.toBytes(key);
        if (!PicX.isHighValues(key)) {
            Optional<Map.Entry<byte[], byte[]>> e = store.ceiling(k);
            if (e.isEmpty() || (equal && !Arrays.equals(e.get().getKey(), k))) {
                return FileResult.of(CicsResp.NOTFND, 80);
            }
        }
        browses.put(reqid, new Browse(k));
        return FileResult.of(CicsResp.NORMAL);
    }

    public FileResult startbr(String key) {
        return startbr(key, false, 0);
    }

    public FileResult readnext(int reqid) {
        Browse b = browses.get(reqid);
        if (b == null) {
            return FileResult.of(CicsResp.INVREQ, 41);
        }
        Optional<Map.Entry<byte[], byte[]>> e;
        if (b.lastKey == null) {
            e = store.ceiling(b.requestedKey);
        } else if (!b.lastWasNext) {
            e = store.ceiling(b.lastKey);
        } else {
            e = store.higher(b.lastKey);
        }
        return browseResult(b, e, true);
    }

    public FileResult readnext() {
        return readnext(0);
    }

    /**
     * READPREV. Immediately after STARTBR it returns the record whose key equals the STARTBR key (or the
     * last record for a HIGH-VALUES key), else NOTFND. After a READNEXT it returns the same record again.
     */
    public FileResult readprev(int reqid) {
        Browse b = browses.get(reqid);
        if (b == null) {
            return FileResult.of(CicsResp.INVREQ, 41);
        }
        Optional<Map.Entry<byte[], byte[]>> e;
        if (b.lastKey == null) {
            if (PicX.isHighValues(PicX.fromBytes(b.requestedKey))) {
                e = store.last();
            } else {
                e = store.get(b.requestedKey).map(r -> Map.entry(b.requestedKey, r));
                if (e.isEmpty()) {
                    return FileResult.of(CicsResp.NOTFND, 80);
                }
            }
        } else if (b.lastWasNext) {
            e = store.get(b.lastKey).map(r -> Map.entry(b.lastKey, r));
        } else {
            e = store.lower(b.lastKey);
        }
        return browseResult(b, e, false);
    }

    public FileResult readprev() {
        return readprev(0);
    }

    public FileResult resetbr(String key, int reqid) {
        if (!browses.containsKey(reqid)) {
            return FileResult.of(CicsResp.INVREQ, 41);
        }
        browses.remove(reqid);
        return startbr(key, false, reqid);
    }

    public FileResult endbr(int reqid) {
        return browses.remove(reqid) == null ? FileResult.of(CicsResp.INVREQ, 41) : FileResult.of(CicsResp.NORMAL);
    }

    public FileResult endbr() {
        return endbr(0);
    }

    /** Opens an alternate-index PATH over this base cluster. */
    public AlternateIndexPath path(String pathName, int altKeyOffset, int altKeyLength, boolean unique) {
        return new AlternateIndexPath(pathName, this, altKeyOffset, altKeyLength, unique);
    }

    Optional<FileResult> checkUsable() {
        return switch (state) {
            case ENABLED -> Optional.empty();
            case DISABLED -> Optional.of(FileResult.of(CicsResp.DISABLED, 50));
            case CLOSED -> Optional.of(FileResult.of(CicsResp.NOTOPEN, 60));
        };
    }

    private FileResult browseResult(Browse b, Optional<Map.Entry<byte[], byte[]>> e, boolean next) {
        if (e.isEmpty()) {
            return FileResult.of(CicsResp.ENDFILE, 90);
        }
        b.lastKey = e.get().getKey();
        b.lastWasNext = next;
        return FileResult.record(CicsResp.NORMAL, e.get().getValue(), PicX.fromBytes(b.lastKey));
    }

    private byte[] keyOf(byte[] record) {
        return Arrays.copyOfRange(record, keyOffset, keyOffset + keyLength);
    }
}
