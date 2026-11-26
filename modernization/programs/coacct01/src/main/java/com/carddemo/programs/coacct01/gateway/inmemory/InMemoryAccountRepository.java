package com.carddemo.programs.coacct01.gateway.inmemory;

import com.carddemo.programs.coacct01.copybook.AccountRecord;
import com.carddemo.programs.coacct01.gateway.AccountRepository;
import com.carddemo.programs.coacct01.gateway.CicsResp;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.LinkedHashMap;
import java.util.Map;

/** ACCTDAT KSDS keyed by the 11-byte ACCT-ID, loadable from the fixed-width ASCII extract. */
public final class InMemoryAccountRepository implements AccountRepository {

    public static final String DATASET = "ACCTDAT ";

    private final Map<String, AccountRecord> records = new LinkedHashMap<>();
    private AccountReadResult forcedResult;
    private int readCount;

    public InMemoryAccountRepository add(AccountRecord record) {
        records.put(record.key(), record);
        return this;
    }

    public static InMemoryAccountRepository load(Reader source) throws IOException {
        InMemoryAccountRepository repo = new InMemoryAccountRepository();
        try (BufferedReader reader = new BufferedReader(source)) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (!line.isBlank()) {
                    repo.add(AccountRecord.parse(line));
                }
            }
        }
        return repo;
    }

    /** Every subsequent READ returns this RESP/RESP2 (e.g. NOTOPEN, DISABLED). */
    public void failWith(int resp, int resp2) {
        forcedResult = new AccountReadResult(resp, resp2, null);
    }

    public int size() {
        return records.size();
    }

    public int readCount() {
        return readCount;
    }

    @Override
    public AccountReadResult read(String dataset, String ridfld) {
        readCount++;
        if (forcedResult != null) {
            return forcedResult;
        }
        if (!DATASET.equals(dataset)) {
            return new AccountReadResult(CicsResp.FILENOTFOUND, 0, null);
        }
        AccountRecord r = records.get(ridfld);
        return r == null
                ? new AccountReadResult(CicsResp.NOTFND, 0, null)
                : new AccountReadResult(CicsResp.NORMAL, 0, r);
    }
}
