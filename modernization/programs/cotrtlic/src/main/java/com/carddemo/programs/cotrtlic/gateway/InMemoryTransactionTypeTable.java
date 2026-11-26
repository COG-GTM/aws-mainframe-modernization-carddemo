package com.carddemo.programs.cotrtlic.gateway;

import com.carddemo.programs.cotrtlic.copybook.DclTransactionType;
import com.carddemo.programs.cotrtlic.support.Cob;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Deque;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * In-memory CARDDEMO.TRANSACTION_TYPE with DB2 cursor semantics (EBCDIC collation on TR_TYPE,
 * LIKE on TR_DESCRIPTION, -501 for a cursor that is not open, -532 for rows with dependents) and
 * per-statement SQLCODE injection for error-path tests.
 */
public final class InMemoryTransactionTypeTable implements TransactionTypeDb2Gateway {

    public enum Statement {
        PRIMING, COUNT, OPEN_FORWARD, FETCH_FORWARD, CLOSE_FORWARD, OPEN_BACKWARD, FETCH_BACKWARD,
        CLOSE_BACKWARD, UPDATE, DELETE
    }

    private final Map<String, String> rows = new LinkedHashMap<>();
    private final Set<String> typesWithChildren = new HashSet<>();
    private final Map<Statement, Deque<Integer>> injected = new EnumMap<>(Statement.class);
    private final Map<Statement, Integer> callCounts = new EnumMap<>(Statement.class);
    private final Map<Statement, Map<Integer, Integer>> failOnCall = new EnumMap<>(Statement.class);
    private Deque<Map.Entry<String, String>> forward;
    private Deque<Map.Entry<String, String>> backward;

    public InMemoryTransactionTypeTable put(String trType, String description) {
        rows.put(Cob.move(trType, 2), description);
        return this;
    }

    /** Marks TR_TYPE as referenced by TRANSACTION_TYPE_CATEGORY (DELETE fails with -532). */
    public InMemoryTransactionTypeTable addChildReference(String trType) {
        typesWithChildren.add(Cob.move(trType, 2));
        return this;
    }

    /** The next execution of {@code statement} returns {@code sqlcode} instead of running. */
    public InMemoryTransactionTypeTable failNext(Statement statement, int sqlcode) {
        injected.computeIfAbsent(statement, s -> new ArrayDeque<>()).add(sqlcode);
        return this;
    }

    /** Execution number {@code callNumber} (1-based, counted from now on) of {@code statement} returns {@code sqlcode}. */
    public InMemoryTransactionTypeTable failOnCall(Statement statement, int callNumber, int sqlcode) {
        failOnCall.computeIfAbsent(statement, s -> new LinkedHashMap<>()).put(calls(statement) + callNumber, sqlcode);
        return this;
    }

    public Map<String, String> snapshot() {
        return new LinkedHashMap<>(rows);
    }

    public String description(String trType) {
        return rows.get(Cob.move(trType, 2));
    }

    public int calls(Statement statement) {
        return callCounts.getOrDefault(statement, 0);
    }

    private Integer injectedCode(Statement s) {
        int n = callCounts.merge(s, 1, Integer::sum);
        Map<Integer, Integer> planned = failOnCall.get(s);
        if (planned != null && planned.containsKey(n)) {
            return planned.remove(n);
        }
        Deque<Integer> q = injected.get(s);
        return q == null || q.isEmpty() ? null : q.poll();
    }

    private static String sqlerrmc(int code) {
        return switch (code) {
            case -501 -> "CURSOR NOT OPEN";
            case -532 -> "FK RELATIONSHIP";
            case -911 -> "00C90088 TIMEOUT OR DEADLOCK";
            default -> "SQLCODE " + code;
        };
    }

    private boolean matches(String trType, String description, TypeFilter f) {
        boolean typeOk = f.editTypeFlag() != '1' || trType.equals(Cob.move(f.typeCdFilter(), 2));
        boolean descOk = f.editDescFlag() != '1' || like(description, db2Trim(f.typeDescFilter()));
        return typeOk && descOk;
    }

    /** DB2 TRIM(): strips leading and trailing blanks. */
    private static String db2Trim(String s) {
        return Cob.trim(s);
    }

    /** SQL LIKE ('%' any run, '_' one character) as an O(n*m) table match, without regex backtracking. */
    static boolean like(String value, String pattern) {
        int n = value.length();
        int m = pattern.length();
        boolean[] prev = new boolean[n + 1];
        boolean[] cur = new boolean[n + 1];
        prev[0] = true;
        for (int j = 1; j <= m; j++) {
            char p = pattern.charAt(j - 1);
            cur[0] = prev[0] && p == '%';
            for (int i = 1; i <= n; i++) {
                cur[i] = switch (p) {
                    case '%' -> cur[i - 1] || prev[i];
                    case '_' -> prev[i - 1];
                    default -> prev[i - 1] && value.charAt(i - 1) == p;
                };
            }
            boolean[] t = prev;
            prev = cur;
            cur = t;
        }
        return prev[n];
    }

    private List<Map.Entry<String, String>> select(TypeFilter f, boolean forwardDirection) {
        String key = Cob.move(f.startKey(), 2);
        List<Map.Entry<String, String>> out = new ArrayList<>();
        for (Map.Entry<String, String> e : rows.entrySet()) {
            int cmp = Cob.compareEbcdic(e.getKey(), key);
            boolean keyOk = forwardDirection ? cmp >= 0 : cmp < 0;
            if (keyOk && matches(e.getKey(), e.getValue(), f)) {
                out.add(Map.entry(e.getKey(), e.getValue()));
            }
        }
        Comparator<Map.Entry<String, String>> byType = (a, b) -> Cob.compareEbcdic(a.getKey(), b.getKey());
        out.sort(forwardDirection ? byType : byType.reversed());
        return out;
    }

    @Override
    public SqlResult<Integer> primingQuery() {
        Integer code = injectedCode(Statement.PRIMING);
        return code != null ? SqlResult.error(code, sqlerrmc(code)) : SqlResult.ok(1);
    }

    @Override
    public SqlResult<Integer> countByFilters(TypeFilter filter) {
        Integer code = injectedCode(Statement.COUNT);
        if (code != null) {
            return SqlResult.error(code, sqlerrmc(code));
        }
        int n = 0;
        for (Map.Entry<String, String> e : rows.entrySet()) {
            if (matches(e.getKey(), e.getValue(), filter)) {
                n++;
            }
        }
        return SqlResult.ok(n);
    }

    @Override
    public SqlResult<Void> openForwardCursor(TypeFilter filter) {
        Integer code = injectedCode(Statement.OPEN_FORWARD);
        if (code != null) {
            return SqlResult.error(code, sqlerrmc(code));
        }
        if (forward != null) {
            return SqlResult.error(-502, "CURSOR ALREADY OPEN");
        }
        forward = new ArrayDeque<>(select(filter, true));
        return SqlResult.ok(null);
    }

    @Override
    public SqlResult<DclTransactionType> fetchForward() {
        return fetch(Statement.FETCH_FORWARD, forward);
    }

    @Override
    public SqlResult<Void> closeForwardCursor() {
        Integer code = injectedCode(Statement.CLOSE_FORWARD);
        if (code != null) {
            forward = null;
            return SqlResult.error(code, sqlerrmc(code));
        }
        if (forward == null) {
            return SqlResult.error(-501, sqlerrmc(-501));
        }
        forward = null;
        return SqlResult.ok(null);
    }

    @Override
    public SqlResult<Void> openBackwardCursor(TypeFilter filter) {
        Integer code = injectedCode(Statement.OPEN_BACKWARD);
        if (code != null) {
            return SqlResult.error(code, sqlerrmc(code));
        }
        if (backward != null) {
            return SqlResult.error(-502, "CURSOR ALREADY OPEN");
        }
        backward = new ArrayDeque<>(select(filter, false));
        return SqlResult.ok(null);
    }

    @Override
    public SqlResult<DclTransactionType> fetchBackward() {
        return fetch(Statement.FETCH_BACKWARD, backward);
    }

    @Override
    public SqlResult<Void> closeBackwardCursor() {
        Integer code = injectedCode(Statement.CLOSE_BACKWARD);
        if (code != null) {
            backward = null;
            return SqlResult.error(code, sqlerrmc(code));
        }
        if (backward == null) {
            return SqlResult.error(-501, sqlerrmc(-501));
        }
        backward = null;
        return SqlResult.ok(null);
    }

    private SqlResult<DclTransactionType> fetch(Statement s, Deque<Map.Entry<String, String>> cursor) {
        Integer code = injectedCode(s);
        if (code != null) {
            return SqlResult.error(code, sqlerrmc(code));
        }
        if (cursor == null) {
            return SqlResult.error(-501, sqlerrmc(-501));
        }
        Map.Entry<String, String> next = cursor.poll();
        if (next == null) {
            return SqlResult.notFound();
        }
        return SqlResult.ok(DclTransactionType.fetched(next.getKey(), next.getValue()));
    }

    @Override
    public SqlResult<Void> updateDescription(DclTransactionType host) {
        Integer code = injectedCode(Statement.UPDATE);
        if (code != null) {
            return SqlResult.error(code, sqlerrmc(code));
        }
        String key = Cob.move(host.trType(), 2);
        if (!rows.containsKey(key)) {
            return SqlResult.notFound();
        }
        rows.put(key, host.varcharValue());
        return SqlResult.ok(null);
    }

    @Override
    public SqlResult<Void> deleteByType(String trType) {
        Integer code = injectedCode(Statement.DELETE);
        if (code != null) {
            return SqlResult.error(code, sqlerrmc(code));
        }
        String key = Cob.move(trType, 2);
        if (typesWithChildren.contains(key)) {
            return SqlResult.error(-532, sqlerrmc(-532));
        }
        if (rows.remove(key) == null) {
            return SqlResult.notFound();
        }
        return SqlResult.ok(null);
    }
}
