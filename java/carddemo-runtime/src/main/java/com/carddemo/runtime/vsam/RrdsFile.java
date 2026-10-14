package com.carddemo.runtime.vsam;

import com.carddemo.runtime.cics.CicsResp;
import com.carddemo.runtime.cobol.PicX;
import java.util.Map;
import java.util.TreeMap;

/** In-memory VSAM RRDS: fixed slots addressed by relative record number (1-based). */
public final class RrdsFile {

    private final String name;
    private final int recordLength;
    private final TreeMap<Long, byte[]> slots = new TreeMap<>();

    public RrdsFile(String name, int recordLength) {
        this.name = name.trim();
        this.recordLength = recordLength;
    }

    public String name() {
        return name;
    }

    public synchronized FileResult write(long rrn, String record) {
        if (rrn < 1) {
            return FileResult.of(CicsResp.INVREQ, 25);
        }
        if (record.length() != recordLength) {
            return FileResult.of(CicsResp.LENGERR, 12);
        }
        if (slots.containsKey(rrn)) {
            return FileResult.of(CicsResp.DUPREC);
        }
        slots.put(rrn, PicX.toBytes(record));
        return new FileResult(CicsResp.NORMAL, 0, PicX.toBytes(record), null, rrn);
    }

    public synchronized FileResult read(long rrn) {
        byte[] rec = slots.get(rrn);
        return rec == null ? FileResult.of(CicsResp.NOTFND, 80) : new FileResult(CicsResp.NORMAL, 0, rec.clone(), null, rrn);
    }

    public synchronized FileResult rewrite(long rrn, String record) {
        if (!slots.containsKey(rrn)) {
            return FileResult.of(CicsResp.NOTFND, 80);
        }
        if (record.length() != recordLength) {
            return FileResult.of(CicsResp.LENGERR, 12);
        }
        slots.put(rrn, PicX.toBytes(record));
        return new FileResult(CicsResp.NORMAL, 0, PicX.toBytes(record), null, rrn);
    }

    public synchronized FileResult delete(long rrn) {
        return slots.remove(rrn) == null ? FileResult.of(CicsResp.NOTFND, 80) : FileResult.of(CicsResp.NORMAL);
    }

    /** First occupied slot with RRN >= {@code rrn} (browse helper). */
    public synchronized FileResult readNextFrom(long rrn) {
        Map.Entry<Long, byte[]> e = slots.ceilingEntry(rrn);
        return e == null ? FileResult.of(CicsResp.ENDFILE, 90) : new FileResult(CicsResp.NORMAL, 0, e.getValue().clone(), null, e.getKey());
    }
}
