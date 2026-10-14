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

    public Optional<KsdsFile> ksds(String dataset) {
        return Optional.ofNullable(ksds.get(dataset.trim()));
    }

    /** Registers an alternate-index PATH (CSD DEFINE FILE over an AIX PATH dataset) under its file name. */
    public FileControl register(AlternateIndexPath path) {
        paths.put(path.name().trim(), path);
        return this;
    }

    public Optional<AlternateIndexPath> path(String dataset) {
        return Optional.ofNullable(paths.get(dataset.trim()));
    }

    /** READ by full key; for a registered AIX PATH the key is the alternate key. */
    public FileResult read(String dataset, String key, int intoLength) {
        Optional<AlternateIndexPath> path = path(dataset);
        if (path.isPresent()) {
            return path.get().read(key, intoLength);
        }
        return ksds(dataset).map(f -> f.read(key, intoLength)).orElse(FileResult.of(CicsResp.FILENOTFOUND, 1));
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
        Optional<AlternateIndexPath> path = path(dataset);
        if (path.isPresent()) {
            return path.get().startbr(key);
        }
        return ksds(dataset).map(f -> f.startbr(key)).orElse(FileResult.of(CicsResp.FILENOTFOUND, 1));
    }

    public FileResult readnext(String dataset) {
        Optional<AlternateIndexPath> path = path(dataset);
        if (path.isPresent()) {
            return path.get().readnext();
        }
        return ksds(dataset).map(KsdsFile::readnext).orElse(FileResult.of(CicsResp.FILENOTFOUND, 1));
    }

    public FileResult readprev(String dataset) {
        return ksds(dataset).map(KsdsFile::readprev).orElse(FileResult.of(CicsResp.FILENOTFOUND, 1));
    }

    public FileResult endbr(String dataset) {
        Optional<AlternateIndexPath> path = path(dataset);
        if (path.isPresent()) {
            return path.get().endbr();
        }
        return ksds(dataset).map(KsdsFile::endbr).orElse(FileResult.of(CicsResp.FILENOTFOUND, 1));
    }
}
