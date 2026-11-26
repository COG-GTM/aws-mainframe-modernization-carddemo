package com.carddemo.programs.copaus0c.inmemory;

import com.carddemo.programs.copaus0c.copybook.AccountRecord;
import com.carddemo.programs.copaus0c.gateway.AccountRepository;
import com.carddemo.programs.copaus0c.gateway.FileReadResult;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

/** In-memory ACCTDAT KSDS keyed by the 11-digit account id. */
public final class InMemoryAccountRepository implements AccountRepository {
    private final Map<String, AccountRecord> records = new HashMap<>();
    private FileReadResult<AccountRecord> forced;

    public InMemoryAccountRepository add(AccountRecord record) {
        records.put(String.format("%011d", record.acctId()), record);
        return this;
    }

    public void forceResponse(int resp, int resp2) {
        forced = new FileReadResult<>(resp, resp2, null);
    }

    public static InMemoryAccountRepository load(Path file) throws IOException {
        InMemoryAccountRepository repo = new InMemoryAccountRepository();
        for (String line : Files.readAllLines(file)) {
            if (!line.isBlank()) {
                repo.add(AccountRecord.parse(line));
            }
        }
        return repo;
    }

    @Override
    public FileReadResult<AccountRecord> read(String acctIdKey) {
        if (forced != null) {
            return forced;
        }
        AccountRecord r = records.get(acctIdKey);
        return r == null ? FileReadResult.notFound() : FileReadResult.normal(r);
    }
}
