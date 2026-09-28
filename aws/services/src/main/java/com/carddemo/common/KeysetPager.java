package com.carddemo.common;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.stereotype.Component;

/**
 * Keyset paging over a single string key column. {@code next} returns keys greater than {@code startKey}
 * ascending; {@code prev} reads keys lower than {@code startKey} descending and re-sorts ascending. Keys are
 * compared with the "C" collation so ordering is byte-wise, as with a VSAM KSDS key.
 */
@Component
public class KeysetPager {

    private final NamedParameterJdbcTemplate jdbc;

    public KeysetPager(NamedParameterJdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    public <T> PageResponse<T> page(String selectFrom, String keyColumn, String filter, Map<String, Object> filterParams,
            PageQuery query, RowMapper<T> mapper, Function<T, String> keyOf) {
        String key = keyColumn + " COLLATE \"C\"";
        String where = filter == null || filter.isBlank() ? "TRUE" : "(" + filter + ")";
        Map<String, Object> params = new HashMap<>(filterParams);
        params.put("pageSize", query.pageSize());
        StringBuilder sql = new StringBuilder(selectFrom).append(" WHERE ").append(where);
        if (query.startKey() != null) {
            params.put("startKey", query.startKey());
            sql.append(" AND ").append(key).append(query.direction() == PageQuery.Direction.NEXT ? " > " : " < ")
                    .append(":startKey");
        }
        sql.append(" ORDER BY ").append(key)
                .append(query.direction() == PageQuery.Direction.NEXT ? " ASC" : " DESC")
                .append(" LIMIT :pageSize");
        List<T> items = new ArrayList<>(jdbc.query(sql.toString(), params, mapper));
        if (query.direction() == PageQuery.Direction.PREV) {
            Collections.reverse(items);
        }
        if (items.isEmpty()) {
            return new PageResponse<>(List.of(), null, null, false, false, null);
        }
        String firstKey = keyOf.apply(items.getFirst());
        String lastKey = keyOf.apply(items.getLast());
        boolean hasPrev = exists(selectFrom, key, where, filterParams, " < ", firstKey);
        boolean hasNext = exists(selectFrom, key, where, filterParams, " > ", lastKey);
        return new PageResponse<>(List.copyOf(items), firstKey, lastKey, hasNext, hasPrev, null);
    }

    private boolean exists(String selectFrom, String key, String where, Map<String, Object> filterParams, String op,
            String boundary) {
        String from = selectFrom.substring(selectFrom.toUpperCase().indexOf(" FROM "));
        Map<String, Object> params = new HashMap<>(filterParams);
        params.put("boundary", boundary);
        Boolean result = jdbc.queryForObject(
                "SELECT EXISTS (SELECT 1" + from + " WHERE " + where + " AND " + key + op + ":boundary)", params,
                Boolean.class);
        return Boolean.TRUE.equals(result);
    }
}
