package com.carddemo.programs.cbpaup0c.ims;

import com.carddemo.programs.cbpaup0c.ImsReturnCode;
import com.carddemo.programs.cbpaup0c.copybook.PendingAuthDetails;
import com.carddemo.programs.cbpaup0c.copybook.PendingAuthSummary;

import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * In-memory DBPAUTP0 with IMS positioning semantics for the calls CBPAUP0C uses. Segments are held as their
 * raw copybook bytes so a GN/GNP always hands the program a fresh working-storage copy. Status codes can be
 * injected per call for error-path tests.
 */
public final class InMemoryPautDb implements PautDbGateway {

    /** A stored segment: copybook bytes plus, when loaded from an unload file, the original unload record. */
    public static final class Segment {
        private final byte[] data;
        private final byte[] sourceRecord;
        private boolean deleted;

        Segment(byte[] data, byte[] sourceRecord) {
            this.data = data.clone();
            this.sourceRecord = sourceRecord == null ? null : sourceRecord.clone();
        }

        public byte[] data() {
            return data.clone();
        }

        public byte[] sourceRecord() {
            return sourceRecord == null ? null : sourceRecord.clone();
        }

        public boolean deleted() {
            return deleted;
        }
    }

    /** A root with its dependents in sequence. */
    public static final class Root {
        private final Segment summary;
        private final List<Segment> details = new ArrayList<>();

        Root(Segment summary) {
            this.summary = summary;
        }

        public Segment summary() {
            return summary;
        }

        public List<Segment> details() {
            return Collections.unmodifiableList(details);
        }
    }

    private final Charset charset;
    private final List<Root> roots = new ArrayList<>();
    private final List<String> checkpointIds = new ArrayList<>();
    private final Map<DliCall, Integer> callCounts = new EnumMap<>(DliCall.class);
    private final Map<DliCall, Map<Integer, String>> injected = new EnumMap<>(DliCall.class);

    private int rootPos = -1;
    private int detailPos = -1;

    public InMemoryPautDb(Charset charset) {
        this.charset = charset;
    }

    public Charset charset() {
        return charset;
    }

    public InMemoryPautDb addSummary(PendingAuthSummary summary) {
        return addSummary(summary.toBytes(charset), null);
    }

    public InMemoryPautDb addSummary(byte[] data, byte[] sourceRecord) {
        roots.add(new Root(new Segment(data, sourceRecord)));
        return this;
    }

    /** Adds a detail under the most recently added summary. */
    public InMemoryPautDb addDetail(PendingAuthDetails detail) {
        return addDetail(detail.toBytes(charset), null);
    }

    public InMemoryPautDb addDetail(byte[] data, byte[] sourceRecord) {
        if (roots.isEmpty()) {
            throw new IllegalStateException("PAUTDTL1 segment without a PAUTSUM0 parent");
        }
        roots.get(roots.size() - 1).details.add(new Segment(data, sourceRecord));
        return this;
    }

    /** Makes the {@code occurrence}-th (1-based) call of the given kind return {@code status} without acting. */
    public InMemoryPautDb injectStatus(DliCall call, int occurrence, String status) {
        injected.computeIfAbsent(call, k -> new HashMap<>()).put(occurrence, status);
        return this;
    }

    public List<Root> roots() {
        return Collections.unmodifiableList(roots);
    }

    public List<String> checkpointIds() {
        return Collections.unmodifiableList(checkpointIds);
    }

    public int callCount(DliCall call) {
        return callCounts.getOrDefault(call, 0);
    }

    public List<PendingAuthSummary> remainingSummaries() {
        List<PendingAuthSummary> out = new ArrayList<>();
        for (Root r : roots) {
            if (!r.summary.deleted) {
                out.add(PendingAuthSummary.fromBytes(r.summary.data, charset));
            }
        }
        return out;
    }

    public List<PendingAuthDetails> remainingDetails(long accountId) {
        List<PendingAuthDetails> out = new ArrayList<>();
        for (Root r : roots) {
            if (!r.summary.deleted
                    && PendingAuthSummary.fromBytes(r.summary.data, charset).accountId() == accountId) {
                for (Segment d : r.details) {
                    if (!d.deleted) {
                        out.add(PendingAuthDetails.fromBytes(d.data, charset));
                    }
                }
            }
        }
        return out;
    }

    private String injectedStatus(DliCall call) {
        int n = callCounts.merge(call, 1, Integer::sum);
        Map<Integer, String> m = injected.get(call);
        return m == null ? null : m.get(n);
    }

    @Override
    public DliResult<PendingAuthSummary> getNextSummary() {
        String forced = injectedStatus(DliCall.GN_PAUTSUM0);
        if (forced != null) {
            return DliResult.status(forced);
        }
        int next = rootPos + 1;
        while (next < roots.size() && roots.get(next).summary.deleted) {
            next++;
        }
        if (next >= roots.size()) {
            rootPos = roots.size();
            detailPos = -1;
            return DliResult.status("GB");
        }
        rootPos = next;
        detailPos = -1;
        return DliResult.of(ImsReturnCode.OK, PendingAuthSummary.fromBytes(roots.get(rootPos).summary.data, charset));
    }

    @Override
    public DliResult<PendingAuthDetails> getNextDetailWithinParent() {
        String forced = injectedStatus(DliCall.GNP_PAUTDTL1);
        if (forced != null) {
            return DliResult.status(forced);
        }
        if (rootPos < 0 || rootPos >= roots.size()) {
            return DliResult.status("GE");
        }
        List<Segment> details = roots.get(rootPos).details;
        int next = detailPos + 1;
        while (next < details.size() && details.get(next).deleted) {
            next++;
        }
        if (next >= details.size()) {
            detailPos = details.size();
            return DliResult.status("GE");
        }
        detailPos = next;
        return DliResult.of(ImsReturnCode.OK, PendingAuthDetails.fromBytes(details.get(detailPos).data, charset));
    }

    @Override
    public String deleteDetail(PendingAuthDetails detail) {
        String forced = injectedStatus(DliCall.DLET_PAUTDTL1);
        if (forced != null) {
            return forced;
        }
        if (rootPos < 0 || rootPos >= roots.size()) {
            return "DJ";
        }
        List<Segment> details = roots.get(rootPos).details;
        if (detailPos < 0 || detailPos >= details.size() || details.get(detailPos).deleted) {
            return "DJ";
        }
        details.get(detailPos).deleted = true;
        return ImsReturnCode.OK;
    }

    @Override
    public String deleteSummary(PendingAuthSummary summary) {
        String forced = injectedStatus(DliCall.DLET_PAUTSUM0);
        if (forced != null) {
            return forced;
        }
        if (rootPos < 0 || rootPos >= roots.size() || roots.get(rootPos).summary.deleted) {
            return "DJ";
        }
        Root root = roots.get(rootPos);
        root.summary.deleted = true;
        root.details.forEach(d -> d.deleted = true);
        return ImsReturnCode.OK;
    }

    @Override
    public String checkpoint(String checkpointId) {
        String forced = injectedStatus(DliCall.CHKP);
        if (forced != null) {
            return forced;
        }
        checkpointIds.add(checkpointId);
        return ImsReturnCode.OK;
    }
}
