package com.carddemo.programs.cobil00c.gateway;

import com.carddemo.programs.cobil00c.copybook.AccountRecord;
import java.util.Collection;
import java.util.EnumMap;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;

/** In-memory ACCTDAT KSDS with optional forced RESP codes for error-path tests. */
public class InMemoryAccountDataGateway implements AccountDataGateway {

    public enum Op { READ, REWRITE }

    private final TreeMap<String, AccountRecord> records = new TreeMap<>();
    private final Map<Op, CicsResp> forced = new EnumMap<>(Op.class);

    public InMemoryAccountDataGateway(Collection<AccountRecord> initial) {
        initial.forEach(r -> records.put(r.key(), r));
    }

    public void force(Op op, CicsResp resp) {
        forced.put(op, resp);
    }

    public Optional<AccountRecord> find(String key) {
        return Optional.ofNullable(records.get(key));
    }

    @Override
    public CicsResponse<AccountRecord> readForUpdate(String acctIdKey) {
        CicsResp f = forced.get(Op.READ);
        if (f != null) {
            return CicsResponse.of(f, 0);
        }
        AccountRecord r = records.get(acctIdKey);
        return r == null ? CicsResponse.of(CicsResp.NOTFND, 0) : CicsResponse.normal(r);
    }

    @Override
    public CicsResponse<Void> rewrite(AccountRecord record) {
        CicsResp f = forced.get(Op.REWRITE);
        if (f != null) {
            return CicsResponse.of(f, 0);
        }
        if (!records.containsKey(record.key())) {
            return CicsResponse.of(CicsResp.NOTFND, 0);
        }
        records.put(record.key(), record);
        return CicsResponse.normal();
    }
}
