package com.carddemo.programs.cousr00c;

import com.carddemo.programs.cousr00c.copybook.SecUserData;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 * In-memory USRSEC KSDS with CICS browse semantics: keys in EBCDIC order, STARTBR GTEQ, a key of
 * all X'FF' positions at end of file, READPREV straight after STARTBR returns the positioned record
 * only on an exact key match (else NOTFND), a direction change re-reads the last record, and
 * browse commands without a successful STARTBR get INVREQ.
 */
public class InMemoryUserSecurityFile implements UserSecurityFile {

    public enum Op { STARTBR, READNEXT, READPREV, ENDBR }

    private enum Dir { NONE, NEXT, PREV }

    private final TreeMap<String, SecUserData> records = new TreeMap<>(Cobol.EBCDIC_ORDER);
    private final Map<Op, Deque<CicsResponse>> injected = new EnumMap<>(Op.class);
    private final List<Op> calls = new ArrayList<>();

    private List<SecUserData> browse;
    private String startKey;
    private int pos;
    private int last;
    private Dir dir;

    public InMemoryUserSecurityFile(Collection<SecUserData> initial) {
        initial.forEach(this::write);
    }

    public final void write(SecUserData record) {
        records.put(record.secUsrId(), record);
    }

    public void delete(String id) {
        records.remove(Cobol.alnum(id, SecUserData.KEY_LENGTH));
    }

    /** Make the next {@code op} return {@code response} (e.g. NOTOPEN, IOERR) instead of executing. */
    public void injectFailure(Op op, CicsResponse response) {
        injected.computeIfAbsent(op, k -> new ArrayDeque<>()).add(response);
    }

    /** Commands issued, in order. */
    public List<Op> calls() {
        return List.copyOf(calls);
    }

    public boolean browseActive() {
        return browse != null;
    }

    private CicsResponse injectedFor(Op op) {
        calls.add(op);
        Deque<CicsResponse> q = injected.get(op);
        return q == null ? null : q.poll();
    }

    @Override
    public CicsResponse startBr(String ridfld) {
        CicsResponse failure = injectedFor(Op.STARTBR);
        browse = null;
        if (failure != null) {
            return failure;
        }
        String key = Cobol.alnum(ridfld, SecUserData.KEY_LENGTH);
        List<SecUserData> snapshot = new ArrayList<>(records.values());
        int p;
        if (Cobol.isAll(key, Cobol.HIGH_VALUE)) {
            p = snapshot.size();
        } else {
            String ceiling = records.ceilingKey(key);
            if (ceiling == null) {
                return CicsResponse.of(CicsResp.NOTFND);
            }
            p = records.headMap(ceiling).size();
        }
        browse = snapshot;
        startKey = key;
        pos = p;
        last = -1;
        dir = Dir.NONE;
        return CicsResponse.NORMAL;
    }

    @Override
    public BrowseRead readNext() {
        CicsResponse failure = injectedFor(Op.READNEXT);
        if (failure != null) {
            return new BrowseRead(failure, null);
        }
        if (browse == null) {
            return new BrowseRead(new CicsResponse(CicsResp.INVREQ, 35), null);
        }
        int idx = switch (dir) {
            case NONE -> pos;
            case NEXT -> last + 1;
            case PREV -> last;
        };
        if (idx >= browse.size()) {
            return BrowseRead.of(CicsResp.ENDFILE);
        }
        last = idx;
        dir = Dir.NEXT;
        return new BrowseRead(CicsResponse.NORMAL, browse.get(idx));
    }

    @Override
    public BrowseRead readPrev() {
        CicsResponse failure = injectedFor(Op.READPREV);
        if (failure != null) {
            return new BrowseRead(failure, null);
        }
        if (browse == null) {
            return new BrowseRead(new CicsResponse(CicsResp.INVREQ, 35), null);
        }
        int idx;
        switch (dir) {
            case NONE -> {
                if (Cobol.isAll(startKey, Cobol.HIGH_VALUE)) {
                    idx = browse.size() - 1;
                } else if (pos < browse.size() && browse.get(pos).secUsrId().equals(startKey)) {
                    idx = pos;
                } else {
                    return BrowseRead.of(CicsResp.NOTFND);
                }
            }
            case PREV -> idx = last - 1;
            default -> idx = last;
        }
        if (idx < 0) {
            return BrowseRead.of(CicsResp.ENDFILE);
        }
        last = idx;
        dir = Dir.PREV;
        return new BrowseRead(CicsResponse.NORMAL, browse.get(idx));
    }

    @Override
    public CicsResponse endBr() {
        CicsResponse failure = injectedFor(Op.ENDBR);
        if (failure != null) {
            return failure;
        }
        if (browse == null) {
            return new CicsResponse(CicsResp.INVREQ, 35);
        }
        browse = null;
        return CicsResponse.NORMAL;
    }
}
