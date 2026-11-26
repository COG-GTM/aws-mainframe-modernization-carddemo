package com.carddemo.programs.cotrtupc.gateway;

import com.carddemo.programs.cotrtupc.copybook.DclTransactionType;
import com.carddemo.programs.cotrtupc.copybook.DclTransactionTypeCategory;
import com.carddemo.programs.cotrtupc.copybook.Sqlca;
import com.carddemo.programs.cotrtupc.copybook.TtypUpdateRecord;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 * In-memory CARDDEMO.TRANSACTION_TYPE with the TRANSACTION_TYPE_CATEGORY foreign key (ON DELETE
 * RESTRICT -> SQLCODE -532) and per-statement SQLCODE injection for error-path tests.
 */
public final class InMemoryTransactionTypeGateway implements TransactionTypeGateway, CicsSyncpoint {

    public enum Statement { SELECT, UPDATE, INSERT, DELETE }

    private final Map<String, String> rows = new TreeMap<>();
    private final List<DclTransactionTypeCategory> categories = new ArrayList<>();
    private final Map<Statement, Sqlca> injected = new EnumMap<>(Statement.class);
    private final Map<Statement, RuntimeException> failures = new EnumMap<>(Statement.class);
    private final List<String> log = new ArrayList<>();
    private int syncpoints;

    public InMemoryTransactionTypeGateway put(String type, String description) {
        rows.put(type, description);
        return this;
    }

    public InMemoryTransactionTypeGateway addCategory(DclTransactionTypeCategory c) {
        categories.add(c);
        return this;
    }

    /** Loads fixed-width TRAN-TYPE records (TTYP-UPDATE-RECORD layout, 60 bytes) as table rows. */
    public InMemoryTransactionTypeGateway loadTranTypeFile(Reader reader) throws IOException {
        try (BufferedReader br = new BufferedReader(reader)) {
            String line;
            while ((line = br.readLine()) != null) {
                if (line.isBlank()) {
                    continue;
                }
                TtypUpdateRecord r = TtypUpdateRecord.parse(line);
                put(r.ttupUpdateTtypType(), r.ttupUpdateTtypTypeDesc().stripTrailing());
            }
        }
        return this;
    }

    /** Loads fixed-width TRAN-CAT records (type X(2), category 9(4), description X(50)) as FK children. */
    public InMemoryTransactionTypeGateway loadTranCatFile(Reader reader) throws IOException {
        try (BufferedReader br = new BufferedReader(reader)) {
            String line;
            while ((line = br.readLine()) != null) {
                if (line.length() < 6) {
                    continue;
                }
                String desc = line.length() > 6 ? line.substring(6, Math.min(56, line.length())).stripTrailing() : "";
                addCategory(DclTransactionTypeCategory.of(line.substring(0, 2), line.substring(2, 6), desc));
            }
        }
        return this;
    }

    /** Next execution of {@code stmt} returns {@code sqlca} without touching the table. */
    public InMemoryTransactionTypeGateway inject(Statement stmt, Sqlca sqlca) {
        injected.put(stmt, sqlca);
        return this;
    }

    /** Next execution of {@code stmt} throws (simulates an abend inside the SQL call). */
    public InMemoryTransactionTypeGateway fail(Statement stmt, RuntimeException e) {
        failures.put(stmt, e);
        return this;
    }

    public Map<String, String> rows() {
        return rows;
    }

    public int syncpoints() {
        return syncpoints;
    }

    public List<String> log() {
        return log;
    }

    @Override
    public void syncpoint() {
        syncpoints++;
        log.add("SYNCPOINT");
    }

    private Sqlca intercept(Statement s) {
        log.add(s.name());
        RuntimeException e = failures.remove(s);
        if (e != null) {
            throw e;
        }
        return injected.remove(s);
    }

    @Override
    public Sqlca select(DclTransactionType dcl) {
        Sqlca forced = intercept(Statement.SELECT);
        if (forced != null) {
            return forced;
        }
        String desc = rows.get(dcl.trType);
        if (desc == null) {
            return Sqlca.notFound();
        }
        dcl.trDescriptionLen = (short) desc.length();
        dcl.trDescriptionText(desc);
        return Sqlca.ok();
    }

    @Override
    public Sqlca update(DclTransactionType dcl) {
        Sqlca forced = intercept(Statement.UPDATE);
        if (forced != null) {
            return forced;
        }
        if (!rows.containsKey(dcl.trType)) {
            return Sqlca.notFound();
        }
        rows.put(dcl.trType, dcl.varcharValue());
        return Sqlca.ok();
    }

    @Override
    public Sqlca insert(DclTransactionType dcl) {
        Sqlca forced = intercept(Statement.INSERT);
        if (forced != null) {
            return forced;
        }
        if (rows.containsKey(dcl.trType)) {
            return Sqlca.of(-803, "1 CARDDEMO.TRANSACTION_TYPE", "23505");
        }
        rows.put(dcl.trType, dcl.varcharValue());
        return Sqlca.ok();
    }

    @Override
    public Sqlca delete(DclTransactionType dcl) {
        Sqlca forced = intercept(Statement.DELETE);
        if (forced != null) {
            return forced;
        }
        boolean hasChildren = categories.stream().anyMatch(c -> c.trcTypeCode.equals(dcl.trType));
        if (hasChildren) {
            return Sqlca.of(-532, "TRC_TYPE_CODE", "23504");
        }
        if (rows.remove(dcl.trType) == null) {
            return Sqlca.notFound();
        }
        return Sqlca.ok();
    }
}
