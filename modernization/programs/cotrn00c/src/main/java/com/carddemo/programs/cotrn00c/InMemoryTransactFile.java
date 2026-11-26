package com.carddemo.programs.cotrn00c;

import com.carddemo.programs.cotrn00c.copybook.CobolText;
import com.carddemo.programs.cotrn00c.copybook.TranRecord;

import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 * In-memory TRANSACT KSDS with CICS browse semantics. Supports one browse at a time; a new STARTBR
 * replaces any browse left open (CICS ends open browses at task end). Failures can be injected per
 * operation for error-path tests.
 */
public final class InMemoryTransactFile implements TransactFileGateway {

    public enum Op { STARTBR, READNEXT, READPREV, ENDBR }

    private enum Direction { NONE, NEXT, PREV }

    private final TreeMap<String, TranRecord> records = new TreeMap<>();
    private final Map<Op, Map<Integer, CicsResponse>> injected = new EnumMap<>(Op.class);
    private final Map<Op, Integer> calls = new EnumMap<>(Op.class);
    private final List<String> log = new ArrayList<>();

    private List<TranRecord> view;
    private boolean browsing;
    private int startPos;
    private boolean startExact;
    private boolean startHighValues;
    private int current;
    private Direction lastDirection;

    public InMemoryTransactFile() {
        for (Op op : Op.values()) {
            injected.put(op, new HashMap<>());
            calls.put(op, 0);
        }
    }

    public InMemoryTransactFile(Collection<TranRecord> initial) {
        this();
        initial.forEach(this::put);
    }

    public void put(TranRecord r) {
        records.put(r.tranId(), r);
    }

    public void remove(String tranId) {
        records.remove(CobolText.fit(tranId, TranRecord.TRAN_ID_LENGTH));
    }

    /** The next {@code op} returns {@code response} instead of executing. */
    public void injectFailure(Op op, CicsResponse response) {
        injectFailureOnCall(op, 1, response);
    }

    /** The {@code n}-th {@code op} from now (1-based) returns {@code response} instead of executing. */
    public void injectFailureOnCall(Op op, int n, CicsResponse response) {
        injected.get(op).put(calls.get(op) + n, response);
    }

    private CicsResponse injected(Op op) {
        int call = calls.merge(op, 1, Integer::sum);
        return injected.get(op).remove(call);
    }

    /** Commands issued, e.g. {@code STARTBR 0000...}, {@code READNEXT NORMAL}. */
    public List<String> log() {
        return List.copyOf(log);
    }

    public boolean isBrowsing() {
        return browsing;
    }

    @Override
    public CicsResponse startBr(String ridfld) {
        CicsResponse inj = injected(Op.STARTBR);
        if (inj != null) {
            log.add("STARTBR " + inj.resp());
            return inj;
        }
        String key = CobolText.fit(ridfld, TranRecord.TRAN_ID_LENGTH);
        view = new ArrayList<>(records.values());
        startHighValues = CobolText.isLowValues(key) ? false : key.chars().allMatch(c -> c == CobolText.HIGH_VALUE);
        if (startHighValues) {
            startPos = view.size();
            startExact = false;
        } else {
            String ceiling = records.ceilingKey(key);
            if (ceiling == null) {
                browsing = false;
                log.add("STARTBR NOTFND");
                return CicsResponse.of(CicsResp.NOTFND);
            }
            startPos = records.headMap(ceiling).size();
            startExact = ceiling.equals(key);
        }
        browsing = true;
        current = -1;
        lastDirection = Direction.NONE;
        log.add("STARTBR NORMAL");
        return CicsResponse.NORMAL;
    }

    @Override
    public ReadResult readNext() {
        CicsResponse inj = injected(Op.READNEXT);
        if (inj != null) {
            log.add("READNEXT " + inj.resp());
            return new ReadResult(inj, null);
        }
        if (!browsing) {
            log.add("READNEXT INVREQ");
            return ReadResult.of(CicsResp.INVREQ);
        }
        int idx = switch (lastDirection) {
            case NONE -> startPos;
            case PREV -> current;
            case NEXT -> current + 1;
        };
        return readAt(idx, Direction.NEXT, "READNEXT");
    }

    @Override
    public ReadResult readPrev() {
        CicsResponse inj = injected(Op.READPREV);
        if (inj != null) {
            log.add("READPREV " + inj.resp());
            return new ReadResult(inj, null);
        }
        if (!browsing) {
            log.add("READPREV INVREQ");
            return ReadResult.of(CicsResp.INVREQ);
        }
        int idx;
        switch (lastDirection) {
            case NONE -> {
                if (startHighValues) {
                    idx = view.size() - 1;
                } else if (!startExact) {
                    log.add("READPREV NOTFND");
                    return ReadResult.of(CicsResp.NOTFND);
                } else {
                    idx = startPos;
                }
            }
            case NEXT -> idx = current;
            default -> idx = current - 1;
        }
        return readAt(idx, Direction.PREV, "READPREV");
    }

    private ReadResult readAt(int idx, Direction dir, String op) {
        if (idx < 0 || idx >= view.size()) {
            log.add(op + " ENDFILE");
            return ReadResult.of(CicsResp.ENDFILE);
        }
        current = idx;
        lastDirection = dir;
        TranRecord r = view.get(idx);
        log.add(op + " NORMAL " + r.tranId());
        return ReadResult.of(r);
    }

    @Override
    public CicsResponse endBr() {
        CicsResponse inj = injected(Op.ENDBR);
        if (inj != null) {
            log.add("ENDBR " + inj.resp());
            return inj;
        }
        if (!browsing) {
            log.add("ENDBR INVREQ");
            return CicsResponse.of(CicsResp.INVREQ);
        }
        browsing = false;
        log.add("ENDBR NORMAL");
        return CicsResponse.NORMAL;
    }
}
