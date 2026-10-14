package com.carddemo.runtime.vsam;

import com.carddemo.runtime.cics.CicsResp;
import com.carddemo.runtime.cobol.PicX;
import java.util.ArrayList;
import java.util.List;

/** In-memory VSAM ESDS: records appended in entry order and addressed by RBA (byte offset). */
public final class EsdsFile {

    private final String name;
    private final List<byte[]> records = new ArrayList<>();
    private final List<Long> rbas = new ArrayList<>();
    private long nextRba;

    public EsdsFile(String name) {
        this.name = name.trim();
    }

    public String name() {
        return name;
    }

    /** WRITE: appends; the result's recordId is the new record's RBA. */
    public synchronized FileResult write(String record) {
        byte[] rec = PicX.toBytes(record);
        long rba = nextRba;
        records.add(rec);
        rbas.add(rba);
        nextRba += rec.length;
        return new FileResult(CicsResp.NORMAL, 0, rec.clone(), null, rba);
    }

    public synchronized FileResult read(long rba) {
        int i = rbas.indexOf(rba);
        if (i < 0) {
            return FileResult.of(CicsResp.NOTFND, 80);
        }
        return new FileResult(CicsResp.NORMAL, 0, records.get(i).clone(), null, rba);
    }

    /** REWRITE in place: the length cannot change (LENGERR otherwise). */
    public synchronized FileResult rewrite(long rba, String record) {
        int i = rbas.indexOf(rba);
        if (i < 0) {
            return FileResult.of(CicsResp.NOTFND, 80);
        }
        if (record.length() != records.get(i).length) {
            return FileResult.of(CicsResp.LENGERR, 12);
        }
        records.set(i, PicX.toBytes(record));
        return new FileResult(CicsResp.NORMAL, 0, records.get(i).clone(), null, rba);
    }

    /** ESDS records cannot be deleted. */
    public FileResult delete(long rba) {
        return FileResult.of(CicsResp.INVREQ, 20);
    }

    /** All records in entry sequence (sequential read). */
    public synchronized List<String> readAll() {
        return records.stream().map(PicX::fromBytes).toList();
    }
}
