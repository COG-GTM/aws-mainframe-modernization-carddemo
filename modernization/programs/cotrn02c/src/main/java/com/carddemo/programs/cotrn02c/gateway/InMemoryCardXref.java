package com.carddemo.programs.cotrn02c.gateway;

import com.carddemo.programs.cotrn02c.copybook.CardXrefRecord;
import com.carddemo.programs.cotrn02c.copybook.Pic;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

/**
 * In-memory CCXREF (unique key card number) and CXACAIX (non-unique AIX on account id). A path READ
 * that finds more than one record for the alternate key returns the first with RESP DUPKEY, as CICS does.
 */
public final class InMemoryCardXref implements CardXrefGateway {

    private final List<CardXrefRecord> records = new ArrayList<>();
    private CicsResp injectedAcct;
    private CicsResp injectedCard;

    public static InMemoryCardXref load(Stream<String> fixedWidthLines) {
        InMemoryCardXref x = new InMemoryCardXref();
        fixedWidthLines.filter(l -> !l.isBlank()).map(CardXrefRecord::parse).forEach(x::put);
        return x;
    }

    public void put(CardXrefRecord r) {
        records.add(r);
    }

    public List<CardXrefRecord> records() {
        return records;
    }

    public void injectAcctRead(CicsResp resp) {
        injectedAcct = resp;
    }

    public void injectCardRead(CicsResp resp) {
        injectedCard = resp;
    }

    @Override
    public ReadResult readByAcctId(long acctId) {
        if (injectedAcct != null) {
            CicsResp r = injectedAcct;
            injectedAcct = null;
            return new ReadResult(r, 0, null);
        }
        List<CardXrefRecord> hits = records.stream().filter(r -> r.acctId() == acctId).toList();
        if (hits.isEmpty()) {
            return new ReadResult(CicsResp.NOTFND, 0, null);
        }
        return new ReadResult(hits.size() > 1 ? CicsResp.DUPKEY : CicsResp.NORMAL, 0, hits.get(0));
    }

    @Override
    public ReadResult readByCardNum(String cardNum) {
        if (injectedCard != null) {
            CicsResp r = injectedCard;
            injectedCard = null;
            return new ReadResult(r, 0, null);
        }
        String key = Pic.x(cardNum, 16);
        return records.stream().filter(r -> r.cardNum().equals(key)).findFirst()
                .map(r -> new ReadResult(CicsResp.NORMAL, 0, r))
                .orElse(new ReadResult(CicsResp.NOTFND, 0, null));
    }
}
