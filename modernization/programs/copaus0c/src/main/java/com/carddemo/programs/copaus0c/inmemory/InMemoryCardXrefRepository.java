package com.carddemo.programs.copaus0c.inmemory;

import com.carddemo.programs.copaus0c.copybook.CardXrefRecord;
import com.carddemo.programs.copaus0c.gateway.CardXrefRepository;
import com.carddemo.programs.copaus0c.gateway.FileReadResult;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/** In-memory CXACAIX: returns the first record (in base-cluster order) for a NONUNIQUEKEY account id. */
public final class InMemoryCardXrefRepository implements CardXrefRepository {
    private final List<CardXrefRecord> records = new ArrayList<>();
    private FileReadResult<CardXrefRecord> forced;

    public InMemoryCardXrefRepository add(CardXrefRecord record) {
        records.add(record);
        return this;
    }

    /** Forces every READ to answer with this RESP/RESP2 (error-path simulation). */
    public void forceResponse(int resp, int resp2) {
        forced = new FileReadResult<>(resp, resp2, null);
    }

    public static InMemoryCardXrefRepository load(Path file) throws IOException {
        InMemoryCardXrefRepository repo = new InMemoryCardXrefRepository();
        for (String line : Files.readAllLines(file)) {
            if (!line.isBlank()) {
                repo.add(CardXrefRecord.parse(line));
            }
        }
        return repo;
    }

    @Override
    public FileReadResult<CardXrefRecord> readByAcctId(String acctIdKey) {
        if (forced != null) {
            return forced;
        }
        return records.stream()
                .filter(r -> String.format("%011d", r.xrefAcctId()).equals(acctIdKey))
                .findFirst()
                .map(FileReadResult::normal)
                .orElseGet(FileReadResult::notFound);
    }
}
