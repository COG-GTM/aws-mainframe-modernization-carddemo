package com.carddemo.batch.refdata;

import com.carddemo.batch.core.CardDemoJob;
import com.carddemo.batch.core.JobFailure;
import com.carddemo.batch.core.JobOutcome;
import com.carddemo.batch.core.JobParams;
import com.carddemo.batch.core.ReturnCode;
import com.carddemo.batch.storage.ObjectStore;
import com.carddemo.batch.storage.S3Keys;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionTemplate;

/**
 * IDCAMS {@code DELETE/DEFINE/REPRO} refresh jobs ({@code TRANTYPE.jcl}, {@code TRANCATG.jcl}, {@code DISCGRP.jcl},
 * {@code TCATBALF.jcl}, {@code ACCTFILE.jcl}, …) → keyed upsert of one table inside one DB transaction.
 *
 * <p>Source: {@code --sourceKey}, else the latest {@code refdata/<table>/} object, else
 * {@code seed/ascii/<file>}. Rows absent from the source are deleted unless referenced (then return code 12).
 */
@Component
public class LoadReferenceDataJob implements CardDemoJob {

    public static final String NAME = "load-reference-data";
    private static final Logger log = LoggerFactory.getLogger(LoadReferenceDataJob.class);

    private final JdbcTemplate jdbc;
    private final TransactionTemplate tx;
    private final ObjectStore store;

    public LoadReferenceDataJob(JdbcTemplate jdbc, TransactionTemplate tx, ObjectStore store) {
        this.jdbc = jdbc;
        this.tx = tx;
        this.store = store;
    }

    @Override
    public String name() {
        return NAME;
    }

    @Override
    public JobOutcome run(JobParams p) {
        TableSpec spec = TableSpec.of(p.require("table")).orElseThrow(() -> new JobFailure(ReturnCode.INPUT_ERROR,
                "Unsupported --table=" + p.params().get("table")));
        String key = p.get("sourceKey")
                .or(() -> store.latest(S3Keys.refdataPrefix(spec.table())))
                .or(() -> spec.seedFile().map(S3Keys::seedAscii))
                .orElseThrow(() -> new JobFailure(ReturnCode.INPUT_ERROR, "No source for " + spec.table()));
        List<Map<String, Object>> rows = parse(spec, key);
        boolean deleteMissing = Boolean.parseBoolean(p.get("deleteMissing").orElse("true"));

        int deleted = tx.execute(status -> {
            upsert(spec, rows);
            return deleteMissing ? deleteAbsent(spec, rows) : 0;
        });
        Map<String, Object> counts = new LinkedHashMap<>();
        counts.put("table", spec.table());
        counts.put("source", store.uri(key));
        counts.put("upserted", rows.size());
        counts.put("deleted", deleted);
        log.info("Loaded {} rows into {} from {} ({} deleted)", rows.size(), spec.table(), store.uri(key), deleted);
        return JobOutcome.ok(counts);
    }

    private List<Map<String, Object>> parse(TableSpec spec, String key) {
        String content = new String(store.get(key), StandardCharsets.US_ASCII);
        List<Map<String, Object>> rows = new ArrayList<>();
        String[] lines = content.split("\n", -1);
        for (int i = 0; i < lines.length; i++) {
            if (lines[i].isBlank()) {
                continue;
            }
            try {
                rows.add(spec.parse(lines[i]));
            } catch (RuntimeException e) {
                throw new JobFailure(ReturnCode.INPUT_ERROR,
                        spec.table() + " record " + (i + 1) + " invalid: " + e.getMessage(), e);
            }
        }
        return rows;
    }

    private void upsert(TableSpec spec, List<Map<String, Object>> rows) {
        if (rows.isEmpty()) {
            return;
        }
        List<String> cols = new ArrayList<>(rows.get(0).keySet());
        String updates = cols.stream().filter(c -> !spec.primaryKey().contains(c))
                .map(c -> c + " = EXCLUDED." + c).collect(Collectors.joining(", "));
        if (spec.versioned()) {
            updates += ", version = " + spec.table() + ".version + 1";
        }
        String sql = "INSERT INTO " + spec.table() + " (" + String.join(", ", cols) + ") VALUES ("
                + cols.stream().map(c -> "?").collect(Collectors.joining(", ")) + ") ON CONFLICT ("
                + String.join(", ", spec.primaryKey()) + ") DO UPDATE SET " + updates;
        jdbc.batchUpdate(sql, rows.stream().map(r -> r.values().toArray()).toList());
    }

    private int deleteAbsent(TableSpec spec, List<Map<String, Object>> rows) {
        Set<String> sourceKeys = new HashSet<>();
        for (Map<String, Object> r : rows) {
            sourceKeys.add(spec.primaryKey().stream().map(k -> String.valueOf(r.get(k)).trim())
                    .collect(Collectors.joining("|")));
        }
        String pkList = String.join(", ", spec.primaryKey());
        List<List<Object>> absent = new ArrayList<>();
        jdbc.query("SELECT " + pkList + " FROM " + spec.table(), rs -> {
            List<Object> key = new ArrayList<>();
            StringBuilder s = new StringBuilder();
            for (int i = 1; i <= spec.primaryKey().size(); i++) {
                Object v = rs.getObject(i);
                key.add(v);
                if (i > 1) {
                    s.append('|');
                }
                s.append(String.valueOf(v).trim());
            }
            if (!sourceKeys.contains(s.toString())) {
                absent.add(key);
            }
        });
        if (absent.isEmpty()) {
            return 0;
        }
        String where = spec.primaryKey().stream().map(k -> k + " = ?").collect(Collectors.joining(" AND "));
        try {
            jdbc.batchUpdate("DELETE FROM " + spec.table() + " WHERE " + where,
                    absent.stream().map(List::toArray).toList());
        } catch (DataIntegrityViolationException e) {
            throw new JobFailure(ReturnCode.DATA_ERROR, "Rows absent from source are still referenced in "
                    + spec.table() + ": " + absent.stream().limit(20).map(Object::toString).toList(), e);
        }
        return absent.size();
    }
}
