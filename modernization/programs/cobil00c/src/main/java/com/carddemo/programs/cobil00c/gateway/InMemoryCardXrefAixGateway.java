package com.carddemo.programs.cobil00c.gateway;

import com.carddemo.programs.cobil00c.copybook.CardXrefRecord;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * In-memory CXACAIX path. The AIX is defined NONUNIQUEKEY (XREFFILE.jcl), so a READ that finds
 * more than one base record for the key returns the first one with RESP DUPKEY.
 */
public class InMemoryCardXrefAixGateway implements CardXrefAixGateway {

    private final List<CardXrefRecord> records;
    private CicsResp forced;

    public InMemoryCardXrefAixGateway(Collection<CardXrefRecord> initial) {
        records = new ArrayList<>(initial);
        records.sort((a, b) -> a.xrefCardNum().compareTo(b.xrefCardNum()));
    }

    public void force(CicsResp resp) {
        forced = resp;
    }

    @Override
    public CicsResponse<CardXrefRecord> readByAcctId(String xrefAcctIdKey) {
        if (forced != null) {
            return CicsResponse.of(forced, 0);
        }
        List<CardXrefRecord> hits = records.stream().filter(r -> r.acctKey().equals(xrefAcctIdKey)).toList();
        if (hits.isEmpty()) {
            return CicsResponse.of(CicsResp.NOTFND, 0);
        }
        if (hits.size() > 1) {
            return new CicsResponse<>(CicsResp.DUPKEY, 0, java.util.Optional.of(hits.get(0)));
        }
        return CicsResponse.normal(hits.get(0));
    }
}
