package com.carddemo.runtime.vsam;

import com.carddemo.runtime.cics.CicsResp;
import com.carddemo.runtime.cobol.PicX;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Read/browse access to a KSDS through an alternate index PATH. Records are ordered by (alternate key,
 * primary key). For non-unique indexes READ and READNEXT return DUPKEY when more records share the
 * alternate key of the record returned.
 */
public final class AlternateIndexPath {

    private final String name;
    private final KsdsFile base;
    private final int altKeyOffset;
    private final int altKeyLength;
    private final boolean unique;
    private List<Map.Entry<byte[], byte[]>> browse;
    private int position = -1;

    AlternateIndexPath(String name, KsdsFile base, int altKeyOffset, int altKeyLength, boolean unique) {
        this.name = name;
        this.base = base;
        this.altKeyOffset = altKeyOffset;
        this.altKeyLength = altKeyLength;
        this.unique = unique;
    }

    public String name() {
        return name;
    }

    public FileResult read(String altKey) {
        Optional<FileResult> unusable = base.checkUsable();
        if (unusable.isPresent()) {
            return unusable.get();
        }
        List<Map.Entry<byte[], byte[]>> matches = sorted().stream().filter(e -> altKey(e).equals(altKey)).toList();
        if (matches.isEmpty()) {
            return FileResult.of(CicsResp.NOTFND, 80);
        }
        CicsResp resp = !unique && matches.size() > 1 ? CicsResp.DUPKEY : CicsResp.NORMAL;
        return FileResult.record(resp, matches.getFirst().getValue(), altKey);
    }

    public FileResult startbr(String altKey) {
        Optional<FileResult> unusable = base.checkUsable();
        if (unusable.isPresent()) {
            return unusable.get();
        }
        browse = sorted();
        position = 0;
        while (position < browse.size() && altKey(browse.get(position)).compareTo(altKey) < 0) {
            position++;
        }
        if (position == browse.size()) {
            browse = null;
            return FileResult.of(CicsResp.NOTFND, 80);
        }
        return FileResult.of(CicsResp.NORMAL);
    }

    public FileResult readnext() {
        if (browse == null) {
            return FileResult.of(CicsResp.INVREQ, 41);
        }
        if (position >= browse.size()) {
            return FileResult.of(CicsResp.ENDFILE, 90);
        }
        Map.Entry<byte[], byte[]> e = browse.get(position++);
        String k = altKey(e);
        boolean more = !unique && position < browse.size() && altKey(browse.get(position)).equals(k);
        return FileResult.record(more ? CicsResp.DUPKEY : CicsResp.NORMAL, e.getValue(), k);
    }

    public FileResult endbr() {
        if (browse == null) {
            return FileResult.of(CicsResp.INVREQ, 41);
        }
        browse = null;
        return FileResult.of(CicsResp.NORMAL);
    }

    private List<Map.Entry<byte[], byte[]>> sorted() {
        return base.store().all().stream()
                .sorted(Comparator.comparing(this::altKey)).toList();
    }

    private String altKey(Map.Entry<byte[], byte[]> e) {
        return PicX.fromBytes(e.getValue()).substring(altKeyOffset, altKeyOffset + altKeyLength);
    }
}
