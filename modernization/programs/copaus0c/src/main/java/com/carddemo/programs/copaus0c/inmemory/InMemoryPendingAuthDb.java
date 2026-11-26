package com.carddemo.programs.copaus0c.inmemory;

import com.carddemo.programs.copaus0c.copybook.PendingAuthDetails;
import com.carddemo.programs.copaus0c.copybook.PendingAuthSummary;
import com.carddemo.programs.copaus0c.gateway.DliResult;
import com.carddemo.programs.copaus0c.gateway.PendingAuthDbGateway;

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * In-memory IMS HIDAM PAUTSUM0 (root) / PAUTDTL1 (child) database with DL/I positioning:
 * GU establishes parentage on a root, GNP walks that root's children in stored order.
 */
public final class InMemoryPendingAuthDb implements PendingAuthDbGateway {

    /** DL/I calls whose DIBSTAT can be overridden for error-path tests. */
    public enum Call { SCHD, TERM, GU, GNP, GNP_WHERE }

    /** DIBSTAT returned by DL/I calls issued when no PSB is scheduled. */
    public static final String PSB_NOT_SCHEDULED = "TH";

    private record Root(PendingAuthSummary summary, List<PendingAuthDetails> children) {
    }

    private final Map<Long, Root> roots = new LinkedHashMap<>();
    private final Map<Call, String> forced = new EnumMap<>(Call.class);
    private final List<String> callLog = new ArrayList<>();
    private boolean psbScheduled;
    private Root parent;
    private int childPos = -1;
    private int syncpoints;

    public InMemoryPendingAuthDb addSummary(PendingAuthSummary summary) {
        roots.put(summary.paAcctId(), new Root(summary, new ArrayList<>()));
        return this;
    }

    public InMemoryPendingAuthDb addDetail(long acctId, PendingAuthDetails detail) {
        roots.get(acctId).children().add(detail);
        return this;
    }

    public void force(Call call, String dibstat) {
        forced.put(call, dibstat);
    }

    /** Simulates a PSB left scheduled by an earlier unit of work in the same task. */
    public void markPsbScheduled() {
        psbScheduled = true;
    }

    public boolean isPsbScheduled() {
        return psbScheduled;
    }

    public int syncpoints() {
        return syncpoints;
    }

    public List<String> callLog() {
        return List.copyOf(callLog);
    }

    @Override
    public String schedulePsb(String psbName) {
        callLog.add("SCHD " + psbName);
        String f = forced.remove(Call.SCHD);
        if (f != null) {
            return f;
        }
        if (psbScheduled) {
            return "TC";
        }
        psbScheduled = true;
        return "  ";
    }

    @Override
    public String terminate() {
        callLog.add("TERM");
        String f = forced.remove(Call.TERM);
        if (f != null) {
            return f;
        }
        releasePsb();
        return "  ";
    }

    @Override
    public DliResult<PendingAuthSummary> getUniqueSummary(long paAcctId) {
        callLog.add("GU PAUTSUM0 " + paAcctId);
        String f = forced.get(Call.GU);
        if (f != null) {
            return DliResult.status(f);
        }
        if (!psbScheduled) {
            return DliResult.status(PSB_NOT_SCHEDULED);
        }
        Root root = roots.get(paAcctId);
        if (root == null) {
            parent = null;
            childPos = -1;
            return DliResult.status("GE");
        }
        parent = root;
        childPos = -1;
        return DliResult.ok(root.summary());
    }

    @Override
    public DliResult<PendingAuthDetails> getNextDetail() {
        callLog.add("GNP PAUTDTL1");
        String f = forced.get(Call.GNP);
        if (f != null) {
            return DliResult.status(f);
        }
        if (!psbScheduled) {
            return DliResult.status(PSB_NOT_SCHEDULED);
        }
        if (parent == null) {
            return DliResult.status("GP");
        }
        if (childPos + 1 >= parent.children().size()) {
            return DliResult.status("GE");
        }
        childPos++;
        return DliResult.ok(parent.children().get(childPos));
    }

    @Override
    public DliResult<PendingAuthDetails> getNextDetailByKey(String paAuthorizationKey) {
        callLog.add("GNP PAUTDTL1 WHERE PAUT9CTS");
        String f = forced.get(Call.GNP_WHERE);
        if (f != null) {
            return DliResult.status(f);
        }
        if (!psbScheduled) {
            return DliResult.status(PSB_NOT_SCHEDULED);
        }
        if (parent == null) {
            return DliResult.status("GP");
        }
        for (int i = childPos + 1; i < parent.children().size(); i++) {
            if (parent.children().get(i).authorizationKey().equals(paAuthorizationKey)) {
                childPos = i;
                return DliResult.ok(parent.children().get(i));
            }
        }
        childPos = parent.children().size();
        return DliResult.status("GE");
    }

    @Override
    public void syncpoint() {
        callLog.add("SYNCPOINT");
        syncpoints++;
        releasePsb();
    }

    private void releasePsb() {
        psbScheduled = false;
        parent = null;
        childPos = -1;
    }
}
