package com.carddemo.runtime.vsam;

import com.carddemo.runtime.cics.CicsResp;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The CICS file table (FCT): files by DATASET/FILE name (trailing spaces ignored). Commands against an
 * undefined file return FILENOTFOUND, as CICS does.
 */
public final class FileControl {

    private final Map<String, KsdsFile> ksds = new ConcurrentHashMap<>();
    private final Map<String, AlternateIndexPath> paths = new ConcurrentHashMap<>();

    public FileControl register(KsdsFile file) {
        ksds.put(file.name(), file);
        return this;
    }

    /** Registers an alternate-index PATH under its own FILE name (e.g. CXACAIX over CARDXREF). */
    public FileControl register(AlternateIndexPath path) {
        paths.put(path.name(), path);
        return this;
    }

    public Optional<AlternateIndexPath> path(String dataset) {
        return Optional.ofNullable(paths.get(dataset.trim()));
    }

    public Optional<KsdsFile> ksds(String dataset) {
        return Optional.ofNullable(ksds.get(dataset.trim()));
    }

    public FileResult read(String dataset, String key, int intoLength) {
        Optional<KsdsFile> file = ksds(dataset);
        if (file.isEmpty()) {
            return path(dataset).map(p -> p.read(key, intoLength)).orElse(FileResult.of(CicsResp.FILENOTFOUND, 1));
        }
        return file.get().read(key, intoLength);
    }

    public FileResult readForUpdate(String dataset, String key) {
        return ksds(dataset).map(f -> f.readForUpdate(key)).orElse(FileResult.of(CicsResp.FILENOTFOUND, 1));
    }

    public FileResult write(String dataset, String record) {
        return ksds(dataset).map(f -> f.write(record)).orElse(FileResult.of(CicsResp.FILENOTFOUND, 1));
    }

    public FileResult rewrite(String dataset, String record) {
        return ksds(dataset).map(f -> f.rewrite(record)).orElse(FileResult.of(CicsResp.FILENOTFOUND, 1));
    }

    public FileResult delete(String dataset, String key) {
        return ksds(dataset).map(f -> f.delete(key)).orElse(FileResult.of(CicsResp.FILENOTFOUND, 1));
    }

    public FileResult startbr(String dataset, String key) {
        return ksds(dataset).map(f -> f.startbr(key)).orElse(FileResult.of(CicsResp.FILENOTFOUND, 1));
    }

    public FileResult readnext(String dataset) {
        return ksds(dataset).map(KsdsFile::readnext).orElse(FileResult.of(CicsResp.FILENOTFOUND, 1));
    }

    public FileResult readprev(String dataset) {
        return ksds(dataset).map(KsdsFile::readprev).orElse(FileResult.of(CicsResp.FILENOTFOUND, 1));
    }

    public FileResult endbr(String dataset) {
        return ksds(dataset).map(KsdsFile::endbr).orElse(FileResult.of(CicsResp.FILENOTFOUND, 1));
    }
}
