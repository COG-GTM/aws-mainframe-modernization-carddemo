package com.carddemo.programs.cotrn02c.gateway;

import com.carddemo.programs.cotrn02c.copybook.Pic;
import com.carddemo.programs.cotrn02c.copybook.TranRecord;

import java.util.ArrayList;
import java.util.List;
import java.util.NavigableMap;
import java.util.TreeMap;
import java.util.stream.Stream;

/** In-memory TRANSACT KSDS with optional one-shot RESP injection per command. */
public final class InMemoryTransactFile implements TransactFileGateway {

    public enum Command { STARTBR, READPREV, WRITE }

    private final NavigableMap<String, TranRecord> records = new TreeMap<>();
    private final java.util.EnumMap<Command, CicsResp> injected = new java.util.EnumMap<>(Command.class);
    private final List<String> journal = new ArrayList<>();
    private String browseNext;
    private boolean browsing;

    public static InMemoryTransactFile load(Stream<String> fixedWidthLines) {
        InMemoryTransactFile f = new InMemoryTransactFile();
        fixedWidthLines.filter(l -> !l.isBlank()).map(TranRecord::parse).forEach(f::put);
        return f;
    }

    public void put(TranRecord r) {
        records.put(r.getTranId(), r.copy());
    }

    public NavigableMap<String, TranRecord> records() {
        return records;
    }

    /** Commands issued, in order (STARTBR/READPREV/ENDBR/WRITE). */
    public List<String> journal() {
        return journal;
    }

    public boolean isBrowsing() {
        return browsing;
    }

    /** Make the next {@code command} return {@code resp} instead of executing. */
    public void inject(Command command, CicsResp resp) {
        injected.put(command, resp);
    }

    @Override
    public Result startBrowse(String key) {
        journal.add("STARTBR");
        CicsResp forced = injected.remove(Command.STARTBR);
        if (forced != null) {
            return Result.of(forced);
        }
        if (Pic.isHighValues(key)) {
            browseNext = records.isEmpty() ? null : records.lastKey();
        } else {
            browseNext = records.ceilingKey(key);
            if (browseNext == null) {
                return Result.of(CicsResp.NOTFND);
            }
        }
        browsing = true;
        return Result.of(CicsResp.NORMAL);
    }

    @Override
    public Result readPrev() {
        journal.add("READPREV");
        CicsResp forced = injected.remove(Command.READPREV);
        if (forced != null) {
            return Result.of(forced);
        }
        if (browseNext == null) {
            return Result.of(CicsResp.ENDFILE);
        }
        TranRecord r = records.get(browseNext).copy();
        browseNext = records.lowerKey(browseNext);
        return new Result(CicsResp.NORMAL, 0, r);
    }

    @Override
    public void endBrowse() {
        journal.add("ENDBR");
        browsing = false;
        browseNext = null;
    }

    @Override
    public Result write(TranRecord record) {
        journal.add("WRITE");
        CicsResp forced = injected.remove(Command.WRITE);
        if (forced != null) {
            return Result.of(forced);
        }
        if (records.containsKey(record.getTranId())) {
            return Result.of(CicsResp.DUPREC);
        }
        records.put(record.getTranId(), record.copy());
        return Result.of(CicsResp.NORMAL);
    }
}
