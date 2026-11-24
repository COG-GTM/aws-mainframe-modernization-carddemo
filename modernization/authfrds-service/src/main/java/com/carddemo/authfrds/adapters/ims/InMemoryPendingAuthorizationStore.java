package com.carddemo.authfrds.adapters.ims;

import com.carddemo.authfrds.domain.AuthKey;
import com.carddemo.authfrds.domain.AuthorizationDetail;
import com.carddemo.authfrds.domain.ims.ImsStatusException;
import com.carddemo.authfrds.domain.ims.PendingAuthorizationStore;
import com.carddemo.authfrds.domain.ims.PendingAuthorizationSummary;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Component;

/**
 * STUB for IMS DBPAUTP0. Not IMS: an in-memory map seeded from the legacy unload
 * (data/EBCDIC/AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat decoded by tools/decode_ims_unload.py).
 * {@link #failNextReplace(String)} lets tests and the demo force a DL/I REPL failure.
 */
@Component
public class InMemoryPendingAuthorizationStore implements PendingAuthorizationStore {

    private final Resource seed;
    private final ObjectMapper json;
    private final Map<Long, PendingAuthorizationSummary> summaries = new ConcurrentHashMap<>();
    private final Map<Long, TreeMap<AuthKey, AuthorizationDetail>> details = new ConcurrentHashMap<>();
    private volatile String failNextReplace;

    public InMemoryPendingAuthorizationStore(@Value("${carddemo.ims-stub.seed}") Resource seed, ObjectMapper json) {
        this.seed = seed;
        this.json = json;
        reset();
    }

    /** Reloads the seed (drops all REPLs done so far). */
    public synchronized void reset() {
        summaries.clear();
        details.clear();
        failNextReplace = null;
        try (InputStream in = seed.getInputStream()) {
            for (JsonNode acct : json.readTree(in)) {
                long id = acct.get("acctId").asLong();
                summaries.put(id, new PendingAuthorizationSummary(id, acct.get("custId").asLong()));
                TreeMap<AuthKey, AuthorizationDetail> m = new TreeMap<>();
                for (JsonNode d : acct.get("authorizations")) {
                    AuthorizationDetail a = json.treeToValue(d, AuthorizationDetail.class);
                    m.put(a.key(), a);
                }
                details.put(id, m);
            }
        } catch (IOException e) {
            throw new IllegalStateException("cannot load IMS stub seed " + seed, e);
        }
    }

    public void failNextReplace(String imsStatus) {
        this.failNextReplace = imsStatus;
    }

    public List<PendingAuthorizationSummary> summaries() {
        return summaries.values().stream().sorted((a, b) -> Long.compare(a.acctId(), b.acctId())).toList();
    }

    @Override
    public Optional<PendingAuthorizationSummary> findSummary(long acctId) {
        return Optional.ofNullable(summaries.get(acctId));
    }

    @Override
    public Optional<AuthorizationDetail> findDetail(long acctId, AuthKey key) {
        return Optional.ofNullable(details.getOrDefault(acctId, new TreeMap<>()).get(key));
    }

    @Override
    public Optional<AuthorizationDetail> findNextDetail(long acctId, AuthKey key) {
        var e = details.getOrDefault(acctId, new TreeMap<>()).higherEntry(key);
        return Optional.ofNullable(e == null ? null : e.getValue());
    }

    @Override
    public List<AuthorizationDetail> listDetails(long acctId) {
        return new ArrayList<>(details.getOrDefault(acctId, new TreeMap<>()).values());
    }

    @Override
    public synchronized void replaceDetail(long acctId, AuthorizationDetail detail) {
        String fail = failNextReplace;
        if (fail != null) {
            failNextReplace = null;
            throw new ImsStatusException(fail);
        }
        TreeMap<AuthKey, AuthorizationDetail> m = details.get(acctId);
        if (m == null || !m.containsKey(detail.key())) {
            throw new ImsStatusException("GE");
        }
        m.put(detail.key(), detail);
    }
}
