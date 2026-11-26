package com.carddemo.programs.coactvwc.repository.inmemory;

import com.carddemo.programs.coactvwc.copybook.AccountRecord;
import com.carddemo.programs.coactvwc.repository.AccountRepository;
import com.carddemo.programs.coactvwc.repository.FileReadResult;
import com.carddemo.programs.coactvwc.support.Cobol;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/** In-memory ACCTDAT KSDS. */
public final class InMemoryAccountRepository implements AccountRepository {
    private final Map<String, AccountRecord> byKey = new HashMap<>();

    public InMemoryAccountRepository(Collection<AccountRecord> records) {
        records.forEach(this::put);
    }

    public void put(AccountRecord r) {
        byKey.put(Cobol.digits(r.acctId(), 11), r);
    }

    @Override
    public FileReadResult<AccountRecord> read(String acctIdX) {
        AccountRecord r = byKey.get(acctIdX);
        return r == null ? FileReadResult.notFound() : FileReadResult.normal(r);
    }
}
