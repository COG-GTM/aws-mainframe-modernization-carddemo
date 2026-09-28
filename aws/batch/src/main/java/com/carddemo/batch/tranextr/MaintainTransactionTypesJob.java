package com.carddemo.batch.tranextr;

import com.carddemo.batch.core.CardDemoJob;
import com.carddemo.batch.core.JobOutcome;
import com.carddemo.batch.core.JobParams;
import com.carddemo.batch.core.ReturnCode;
import com.carddemo.batch.record.Fixed;
import com.carddemo.batch.storage.ObjectStore;
import com.carddemo.batch.storage.S3Keys;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.support.TransactionTemplate;

/**
 * {@code MNTTRDB2.jcl} → {@code COBTUPDT}: applies {@code A}/{@code U}/{@code D} maintenance records
 * (col 1 action, cols 2–3 type, cols 4–53 description; {@code *} = comment) to {@code transaction_type}.
 * Like the COBOL {@code 9999-ABEND} paragraph (which only sets {@code RETURN-CODE 4} and continues), an invalid
 * action, a missing row on update/delete or an SQL error is logged, sets return code 4, and processing continues.
 */
@Component
public class MaintainTransactionTypesJob implements CardDemoJob {

    private static final Logger log = LoggerFactory.getLogger(MaintainTransactionTypesJob.class);

    private final JdbcTemplate jdbc;
    private final TransactionTemplate tx;
    private final TransactionTemplate nested;
    private final ObjectStore store;
    private final ObjectMapper json;

    public MaintainTransactionTypesJob(JdbcTemplate jdbc, TransactionTemplate tx, ObjectStore store,
            ObjectMapper json) {
        this.jdbc = jdbc;
        this.tx = tx;
        this.nested = new TransactionTemplate(tx.getTransactionManager());
        this.nested.setPropagationBehavior(TransactionDefinition.PROPAGATION_NESTED);
        this.store = store;
        this.json = json;
    }

    @Override
    public String name() {
        return "maintain-transaction-types";
    }

    @Override
    public JobOutcome run(JobParams p) {
        String key = p.get("inputKey").orElse(S3Keys.transactionTypeMaintenance(p.businessDate()));
        Optional<String> applied = appliedCounts(p);
        if (applied.isPresent()) {
            log.info("runId {} already applied {}: returning recorded counts", p.runId(), key);
            return outcome(readCounts(applied.get()), List.of());
        }
        String content = new String(store.get(key), StandardCharsets.US_ASCII);
        return tx.execute(status -> apply(p, content));
    }

    /**
     * Applies the whole file in one transaction (each record in its own savepoint, so a failed record does not undo
     * the others) and records the counts on the run row in that same transaction: a retry after a crash either
     * replays a rolled-back file or returns the recorded counts, never re-applies committed records.
     */
    private JobOutcome apply(JobParams p, String content) {
        int added = 0;
        int updated = 0;
        int deleted = 0;
        int comments = 0;
        List<String> errors = new ArrayList<>();
        for (String raw : content.split("\n", -1)) {
            if (raw.isBlank()) {
                continue;
            }
            String rec = Fixed.normalize(raw, 53);
            char action = rec.charAt(0);
            String type = rec.substring(1, 3);
            String desc = Fixed.rtrim(rec.substring(3, 53));
            try {
                switch (action) {
                    case 'A' -> {
                        nested.executeWithoutResult(s -> jdbc.update(
                                "INSERT INTO transaction_type (type_cd, description) VALUES (?, ?)", type, desc));
                        added++;
                    }
                    case 'U' -> {
                        int n = nested.execute(s -> jdbc.update(
                                "UPDATE transaction_type SET description = ?, version = version + 1 WHERE type_cd = ?",
                                desc, type));
                        if (n == 0) {
                            errors.add(type + ": No records found.");
                        } else {
                            updated++;
                        }
                    }
                    case 'D' -> {
                        int n = nested.execute(s -> jdbc.update("DELETE FROM transaction_type WHERE type_cd = ?", type));
                        if (n == 0) {
                            errors.add(type + ": No records found.");
                        } else {
                            deleted++;
                        }
                    }
                    case '*' -> comments++;
                    default -> errors.add("ERROR: TYPE NOT VALID: " + action);
                }
            } catch (DataAccessException e) {
                errors.add(type + ": Error accessing TRANSACTION_TYPE table: " + e.getMostSpecificCause().getMessage());
            }
        }
        errors.forEach(e -> log.warn("{}", e));
        Map<String, Object> counts = new LinkedHashMap<>();
        counts.put("added", added);
        counts.put("updated", updated);
        counts.put("deleted", deleted);
        counts.put("comments", comments);
        counts.put("errors", errors.size());
        Map<String, Object> marker = new LinkedHashMap<>(counts);
        marker.put("applied", true);
        try {
            jdbc.update("UPDATE batch_job_run SET counts = COALESCE(counts, '{}'::jsonb) || ?::jsonb"
                    + " WHERE run_id = ? AND job_name = ?", json.writeValueAsString(marker), p.runId(), name());
        } catch (JsonProcessingException e) {
            throw new IllegalStateException(e);
        }
        return outcome(counts, errors);
    }

    private Optional<String> appliedCounts(JobParams p) {
        return jdbc.queryForList("SELECT counts::text FROM batch_job_run WHERE run_id = ? AND job_name = ?"
                + " AND (counts->>'applied')::boolean", String.class, p.runId(), name()).stream().findFirst();
    }

    private Map<String, Object> readCounts(String stored) {
        try {
            Map<String, Object> counts = json.readValue(stored, new TypeReference<LinkedHashMap<String, Object>>() {
            });
            counts.remove("applied");
            return counts;
        } catch (JsonProcessingException e) {
            throw new IllegalStateException(e);
        }
    }

    private static JobOutcome outcome(Map<String, Object> counts, List<String> errors) {
        int errorCount = ((Number) counts.get("errors")).intValue();
        if (errorCount == 0) {
            return JobOutcome.ok(counts);
        }
        String message = errors.isEmpty() ? errorCount + " maintenance record(s) in error" : String.join("; ", errors);
        return JobOutcome.of(ReturnCode.WARNING, counts, message);
    }
}
