package com.carddemo.programs.cobtupdt;

import com.carddemo.programs.cobtupdt.copybook.DclTransactionType;
import com.carddemo.programs.cobtupdt.copybook.Pic;

import java.util.ArrayDeque;
import java.util.Collections;
import java.util.Deque;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;

/**
 * In-memory TRANSACTION_TYPE table (PRIMARY KEY TR_TYPE) returning DB2 SQLCODEs:
 * duplicate key on INSERT = -803, no row on searched UPDATE/DELETE = +100.
 */
public final class InMemoryTransactionTypeGateway implements TransactionTypeGateway {

    public static final int SQLCODE_DUPLICATE_KEY = -803;

    private final Map<String, String> rows = new TreeMap<>();
    private final Deque<Integer> forcedSqlcodes = new ArrayDeque<>();

    /** Makes the next statement return {@code sqlcode} without touching the table (fault injection). */
    public InMemoryTransactionTypeGateway failNextWith(int sqlcode) {
        forcedSqlcodes.add(sqlcode);
        return this;
    }

    public InMemoryTransactionTypeGateway seed(String trType, String trDescription) {
        rows.put(key(trType), varchar(trDescription));
        return this;
    }

    public Optional<String> find(String trType) {
        return Optional.ofNullable(rows.get(key(trType)));
    }

    public Optional<DclTransactionType> fetch(String trType) {
        return find(trType).map(d -> DclTransactionType.of(key(trType), d));
    }

    public Map<String, String> rows() {
        return Collections.unmodifiableMap(rows);
    }

    @Override
    public int insert(String trType, String trDescription) {
        if (!forcedSqlcodes.isEmpty()) {
            return forcedSqlcodes.poll();
        }
        String k = key(trType);
        if (rows.containsKey(k)) {
            return SQLCODE_DUPLICATE_KEY;
        }
        rows.put(k, varchar(trDescription));
        return 0;
    }

    @Override
    public int update(String trType, String trDescription) {
        if (!forcedSqlcodes.isEmpty()) {
            return forcedSqlcodes.poll();
        }
        String k = key(trType);
        if (!rows.containsKey(k)) {
            return 100;
        }
        rows.put(k, varchar(trDescription));
        return 0;
    }

    @Override
    public int delete(String trType) {
        if (!forcedSqlcodes.isEmpty()) {
            return forcedSqlcodes.poll();
        }
        return rows.remove(key(trType)) == null ? 100 : 0;
    }

    private static String key(String trType) {
        return Pic.alphanumeric(trType, DclTransactionType.TR_TYPE_LENGTH);
    }

    private static String varchar(String value) {
        return value.length() > DclTransactionType.TR_DESCRIPTION_MAX_LENGTH
                ? value.substring(0, DclTransactionType.TR_DESCRIPTION_MAX_LENGTH)
                : value;
    }
}
