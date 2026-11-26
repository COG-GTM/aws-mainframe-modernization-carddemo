package com.carddemo.programs.copaus1c.inmemory;

import com.carddemo.programs.copaus1c.ImsStatus;
import com.carddemo.programs.copaus1c.copybook.AuthorizationKey;
import com.carddemo.programs.copaus1c.copybook.PendingAuthDetails;
import com.carddemo.programs.copaus1c.copybook.PendingAuthSummary;
import com.carddemo.programs.copaus1c.gateway.DliResult;
import com.carddemo.programs.copaus1c.gateway.PendingAuthDbGateway;
import com.carddemo.programs.copaus1c.gateway.SyncpointGateway;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.Deque;
import java.util.EnumMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * In-memory stand-in for the HIDAM pending-authorization database (root PAUTSUM0 keyed by ACCNTID, child
 * PAUTDTL1 keyed by PAUT9CTS) plus CICS unit-of-work control. Keeps IMS position (parent + current child),
 * PSB scheduling state, and a journal so SYNCPOINT ROLLBACK undoes REPLs. One instance models the PCB of a
 * single CICS task at a time (as each CICS task schedules its own PSB); it is not meant for concurrent tasks.
 */
public final class InMemoryPendingAuthDb implements PendingAuthDbGateway, SyncpointGateway {

    /** DL/I operations whose next status can be forced for error-path tests. */
    public enum Op { SCHD, TERM, GU, GNP_WHERE, GNP, REPL }

    private static final class Root {
        final PendingAuthSummary summary;
        final List<PendingAuthDetails> children = new ArrayList<>();

        Root(PendingAuthSummary summary) {
            this.summary = summary;
        }
    }

    private record JournalEntry(Root root, int index, PendingAuthDetails before) {
    }

    private final Map<Long, Root> roots = new LinkedHashMap<>();
    private final EnumMap<Op, Deque<String>> forced = new EnumMap<>(Op.class);
    private final List<JournalEntry> journal = new ArrayList<>();
    private final List<String> calls = new ArrayList<>();
    private boolean psbScheduled;
    private Root currentRoot;
    private int currentChild = -1;
    private int syncpoints;
    private int rollbacks;

    public void addSummary(PendingAuthSummary summary) {
        roots.put(summary.getAcctId(), new Root(summary.copy()));
    }

    /** Insert a child under its root, keeping twin order ascending on the PAUT9CTS key. */
    public void addDetail(long acctId, PendingAuthDetails details) {
        Root root = roots.get(acctId);
        if (root == null) {
            throw new IllegalArgumentException("No PAUTSUM0 root for account " + acctId);
        }
        root.children.add(details.copy());
        root.children.sort(Comparator.comparing(PendingAuthDetails::getAuthorizationKey));
    }

    public void forceNext(Op op, String dibstat) {
        forced.computeIfAbsent(op, k -> new ArrayDeque<>()).add(dibstat);
    }

    /** A forced successful status (spaces/FW) still performs the call; only the returned status is replaced. */
    private static String okOr(String forced) {
        return forced != null ? forced : ImsStatus.OK;
    }

    private String forcedStatus(Op op) {
        Deque<String> q = forced.get(op);
        return q == null ? null : q.poll();
    }

    @Override
    public String schedulePsb(String psbName) {
        calls.add("SCHD " + psbName);
        String f = forcedStatus(Op.SCHD);
        if (f != null) {
            if (ImsStatus.isStatusOk(f)) {
                psbScheduled = true;
            }
            return f;
        }
        if (psbScheduled) {
            return ImsStatus.PSB_SCHEDULED_MORE_THAN_ONCE;
        }
        psbScheduled = true;
        return ImsStatus.OK;
    }

    @Override
    public String terminate() {
        calls.add("TERM");
        String f = forcedStatus(Op.TERM);
        releasePsb();
        return f != null ? f : ImsStatus.OK;
    }

    @Override
    public DliResult<PendingAuthSummary> getUniqueSummary(long acctId) {
        calls.add("GU PAUTSUM0 " + acctId);
        String f = forcedStatus(Op.GU);
        if (f != null && !ImsStatus.isStatusOk(f)) {
            return DliResult.of(f);
        }
        if (!psbScheduled) {
            return DliResult.of(ImsStatus.PSB_NOT_SCHEDULED);
        }
        Root root = roots.get(acctId);
        if (root == null) {
            currentRoot = null;
            currentChild = -1;
            return DliResult.of(ImsStatus.SEGMENT_NOT_FOUND);
        }
        currentRoot = root;
        currentChild = -1;
        return new DliResult<>(okOr(f), root.summary.copy());
    }

    @Override
    public DliResult<PendingAuthDetails> getNextDetailWhere(AuthorizationKey key) {
        calls.add("GNP PAUTDTL1 " + key);
        String f = forcedStatus(Op.GNP_WHERE);
        if (f != null && !ImsStatus.isStatusOk(f)) {
            return DliResult.of(f);
        }
        if (!psbScheduled) {
            return DliResult.of(ImsStatus.PSB_NOT_SCHEDULED);
        }
        if (currentRoot == null) {
            return DliResult.of(ImsStatus.WRONG_PARENTAGE);
        }
        for (int i = currentChild + 1; i < currentRoot.children.size(); i++) {
            if (currentRoot.children.get(i).getAuthorizationKey().equals(key)) {
                currentChild = i;
                return new DliResult<>(okOr(f), currentRoot.children.get(i).copy());
            }
        }
        return DliResult.of(ImsStatus.SEGMENT_NOT_FOUND);
    }

    @Override
    public DliResult<PendingAuthDetails> getNextDetail() {
        calls.add("GNP PAUTDTL1");
        String f = forcedStatus(Op.GNP);
        if (f != null && !ImsStatus.isStatusOk(f)) {
            return DliResult.of(f);
        }
        if (!psbScheduled) {
            return DliResult.of(ImsStatus.PSB_NOT_SCHEDULED);
        }
        if (currentRoot == null) {
            return DliResult.of(ImsStatus.WRONG_PARENTAGE);
        }
        if (currentChild + 1 < currentRoot.children.size()) {
            currentChild++;
            return new DliResult<>(okOr(f), currentRoot.children.get(currentChild).copy());
        }
        return DliResult.of(ImsStatus.SEGMENT_NOT_FOUND);
    }

    @Override
    public String replaceDetail(PendingAuthDetails details) {
        calls.add("REPL PAUTDTL1 " + details.getAuthorizationKey());
        String f = forcedStatus(Op.REPL);
        if (f != null && !ImsStatus.isStatusOk(f)) {
            return f;
        }
        if (!psbScheduled) {
            return ImsStatus.PSB_NOT_SCHEDULED;
        }
        if (currentRoot == null || currentChild < 0) {
            return "DJ";
        }
        PendingAuthDetails before = currentRoot.children.get(currentChild);
        if (!before.getAuthorizationKey().equals(details.getAuthorizationKey())) {
            return "DA";
        }
        journal.add(new JournalEntry(currentRoot, currentChild, before));
        currentRoot.children.set(currentChild, details.copy());
        return okOr(f);
    }

    @Override
    public void syncpoint() {
        calls.add("SYNCPOINT");
        syncpoints++;
        journal.clear();
        releasePsb();
    }

    @Override
    public void rollback() {
        calls.add("SYNCPOINT ROLLBACK");
        rollbacks++;
        for (int i = journal.size() - 1; i >= 0; i--) {
            JournalEntry e = journal.get(i);
            e.root.children.set(e.index, e.before);
        }
        journal.clear();
        releasePsb();
    }

    private void releasePsb() {
        psbScheduled = false;
        currentRoot = null;
        currentChild = -1;
    }

    public PendingAuthDetails findDetail(long acctId, AuthorizationKey key) {
        Root root = roots.get(acctId);
        if (root == null) {
            return null;
        }
        return root.children.stream().filter(d -> d.getAuthorizationKey().equals(key)).findFirst()
                .map(PendingAuthDetails::copy).orElse(null);
    }

    public List<PendingAuthDetails> details(long acctId) {
        Root root = roots.get(acctId);
        return root == null ? List.of() : root.children.stream().map(PendingAuthDetails::copy).toList();
    }

    public Collection<PendingAuthSummary> summaries() {
        return roots.values().stream().map(r -> r.summary.copy()).toList();
    }

    public boolean isPsbScheduled() { return psbScheduled; }
    public int syncpointCount() { return syncpoints; }
    public int rollbackCount() { return rollbacks; }
    public List<String> calls() { return List.copyOf(calls); }
}
