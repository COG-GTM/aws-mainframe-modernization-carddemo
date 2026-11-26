package com.carddemo.programs.copaua0c.gateway.inmemory;

import com.carddemo.programs.copaua0c.copybook.ImsReturnCode;
import com.carddemo.programs.copaua0c.copybook.PendingAuthDetails;
import com.carddemo.programs.copaua0c.copybook.PendingAuthSummary;
import com.carddemo.programs.copaua0c.gateway.ImsGateway;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/** In-memory PAUTSUM0/PAUTDTL1 hierarchy with injectable DIBSTAT failures. */
public final class InMemoryImsGateway implements ImsGateway {

    private final Map<Long, PendingAuthSummary> summaries = new LinkedHashMap<>();
    private final Map<Long, List<PendingAuthDetails>> details = new LinkedHashMap<>();
    private final List<String> calls = new ArrayList<>();
    private boolean scheduled;
    private final List<String> scheduleStatuses = new ArrayList<>();
    private String getStatus;
    private String replStatus;
    private String isrtSummaryStatus;
    private String isrtDetailStatus;

    public InMemoryImsGateway putSummary(PendingAuthSummary s) {
        summaries.put(s.acctId(), s.copy());
        return this;
    }

    public PendingAuthSummary summary(long acctId) {
        return summaries.get(acctId);
    }

    public List<PendingAuthDetails> details(long acctId) {
        return details.getOrDefault(acctId, List.of());
    }

    public List<String> calls() {
        return calls;
    }

    /** Statuses returned by successive SCHD calls (then OK, or TC if already scheduled). */
    public void scheduleStatuses(String... statuses) {
        scheduleStatuses.addAll(List.of(statuses));
    }

    public void getStatus(String s) { getStatus = s; }
    public void replStatus(String s) { replStatus = s; }
    public void isrtSummaryStatus(String s) { isrtSummaryStatus = s; }
    public void isrtDetailStatus(String s) { isrtDetailStatus = s; }

    /** Called by the test syncpoint: SYNCPOINT releases the PSB. */
    public void release() {
        scheduled = false;
    }

    @Override
    public String schedule(String psbName) {
        calls.add("SCHD " + psbName);
        String st;
        if (!scheduleStatuses.isEmpty()) {
            st = scheduleStatuses.remove(0);
        } else {
            st = scheduled ? ImsReturnCode.PSB_SCHEDULED_MORE_THAN_ONCE : ImsReturnCode.OK;
        }
        if (ImsReturnCode.isStatusOk(st)) {
            scheduled = true;
        }
        return st;
    }

    @Override
    public String terminate() {
        calls.add("TERM");
        scheduled = false;
        return ImsReturnCode.OK;
    }

    @Override
    public GetResult getUniqueSummary(long acctId) {
        calls.add("GU PAUTSUM0 " + acctId);
        PendingAuthSummary s = summaries.get(acctId);
        if (getStatus != null && (!ImsReturnCode.isStatusOk(getStatus) || s == null)) {
            return new GetResult(getStatus, null);
        }
        if (getStatus != null) {
            return new GetResult(getStatus, s.copy());
        }
        return s == null ? new GetResult(ImsReturnCode.SEGMENT_NOT_FOUND, null)
                : new GetResult(ImsReturnCode.OK, s.copy());
    }

    @Override
    public String replaceSummary(PendingAuthSummary summary) {
        calls.add("REPL PAUTSUM0 " + summary.acctId());
        if (replStatus != null && !ImsReturnCode.isStatusOk(replStatus)) {
            return replStatus;
        }
        summaries.put(summary.acctId(), summary.copy());
        return replStatus != null ? replStatus : ImsReturnCode.OK;
    }

    @Override
    public String insertSummary(PendingAuthSummary summary) {
        calls.add("ISRT PAUTSUM0 " + summary.acctId());
        if (isrtSummaryStatus != null && !ImsReturnCode.isStatusOk(isrtSummaryStatus)) {
            return isrtSummaryStatus;
        }
        if (summaries.containsKey(summary.acctId())) {
            return ImsReturnCode.DUPLICATE_SEGMENT_FOUND;
        }
        summaries.put(summary.acctId(), summary.copy());
        return isrtSummaryStatus != null ? isrtSummaryStatus : ImsReturnCode.OK;
    }

    @Override
    public String insertDetail(long acctId, PendingAuthDetails d) {
        calls.add("ISRT PAUTDTL1 " + acctId);
        if (isrtDetailStatus != null && !ImsReturnCode.isStatusOk(isrtDetailStatus)) {
            return isrtDetailStatus;
        }
        if (!summaries.containsKey(acctId)) {
            return ImsReturnCode.SEGMENT_NOT_FOUND;
        }
        List<PendingAuthDetails> children = details.computeIfAbsent(acctId, k -> new ArrayList<>());
        boolean duplicateKey = children.stream()
                .anyMatch(c -> c.authDate9c() == d.authDate9c() && c.authTime9c() == d.authTime9c());
        if (duplicateKey) {
            return ImsReturnCode.DUPLICATE_SEGMENT_FOUND;
        }
        children.add(d.copy());
        return isrtDetailStatus != null ? isrtDetailStatus : ImsReturnCode.OK;
    }
}
