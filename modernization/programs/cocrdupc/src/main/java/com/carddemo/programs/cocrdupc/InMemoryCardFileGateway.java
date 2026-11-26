package com.carddemo.programs.cocrdupc;

import com.carddemo.programs.cocrdupc.copybook.CardRecord;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;

/** In-memory KSDS for CARDDAT with injectable failure responses for tests. */
public final class InMemoryCardFileGateway implements CardFileGateway {

    private final Map<String, CardRecord> records = new TreeMap<>();
    private final Deque<FileResult> readFailures = new ArrayDeque<>();
    private final Deque<FileResult> readForUpdateFailures = new ArrayDeque<>();
    private final Deque<FileResult> rewriteFailures = new ArrayDeque<>();
    private final Set<String> lockedKeys = new HashSet<>();
    private CardRecord changeBeforeUpdate;

    public static InMemoryCardFileGateway fromFixedWidthFile(Path file) throws IOException {
        InMemoryCardFileGateway g = new InMemoryCardFileGateway();
        for (String line : Files.readAllLines(file, StandardCharsets.US_ASCII)) {
            if (!line.isBlank()) {
                g.put(CardRecord.parse(line));
            }
        }
        return g;
    }

    public void put(CardRecord r) {
        records.put(r.cardNum(), r);
    }

    public Optional<CardRecord> get(String cardNum) {
        return Optional.ofNullable(records.get(Cobol.alnum(cardNum, 16)));
    }

    public int size() {
        return records.size();
    }

    public void failNextRead(CicsResp resp, int resp2) {
        readFailures.add(FileResult.of(resp, resp2));
    }

    public void failNextReadForUpdate(CicsResp resp, int resp2) {
        readForUpdateFailures.add(FileResult.of(resp, resp2));
    }

    public void failNextRewrite(CicsResp resp, int resp2) {
        rewriteFailures.add(FileResult.of(resp, resp2));
    }

    /** Simulates another task updating the record between the browse and READ UPDATE. */
    public void changeBeforeNextReadForUpdate(CardRecord r) {
        changeBeforeUpdate = r;
    }

    public boolean isLocked() {
        return !lockedKeys.isEmpty();
    }

    public boolean isLocked(String cardNum) {
        return lockedKeys.contains(Cobol.alnum(cardNum, 16));
    }

    @Override
    public FileResult read(String cardNum) {
        if (!readFailures.isEmpty()) {
            return readFailures.poll();
        }
        CardRecord r = records.get(cardNum);
        return r == null ? FileResult.of(CicsResp.NOTFND, 80) : FileResult.normal(r);
    }

    @Override
    public FileResult readForUpdate(String cardNum) {
        if (!readForUpdateFailures.isEmpty()) {
            return readForUpdateFailures.poll();
        }
        if (changeBeforeUpdate != null) {
            put(changeBeforeUpdate);
            changeBeforeUpdate = null;
        }
        CardRecord r = records.get(cardNum);
        if (r == null) {
            return FileResult.of(CicsResp.NOTFND, 80);
        }
        lockedKeys.add(cardNum);
        return FileResult.normal(r);
    }

    @Override
    public FileResult rewrite(CardRecord record) {
        if (!rewriteFailures.isEmpty()) {
            return rewriteFailures.poll();
        }
        if (!lockedKeys.remove(record.cardNum())) {
            return FileResult.of(CicsResp.INVREQ, lockedKeys.isEmpty() ? 30 : 32);
        }
        records.put(record.cardNum(), record);
        return FileResult.normal(record);
    }

    /** Task end: every lock still held is released (the gateway is a single-terminal test double). */
    @Override
    public void syncpoint() {
        lockedKeys.clear();
    }
}
