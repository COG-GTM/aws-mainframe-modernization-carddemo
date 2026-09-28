package com.carddemo.batch.combtran;

import com.carddemo.batch.core.CardDemoJob;
import com.carddemo.batch.core.JobFailure;
import com.carddemo.batch.core.JobFailureWithCounts;
import com.carddemo.batch.core.JobOutcome;
import com.carddemo.batch.core.JobParams;
import com.carddemo.batch.core.ReturnCode;
import com.carddemo.batch.record.TransactionRecord;
import com.carddemo.batch.storage.ObjectStore;
import com.carddemo.batch.storage.S3Keys;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UncheckedIOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.zip.GZIPInputStream;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

/**
 * {@code COMBTRAN.jcl} (SORT of {@code TRANSACT.BKUP(0)} + {@code SYSTRAN(0)} by {@code TRAN-ID}, then IDCAMS
 * REPRO into {@code TRANSACT}). On AWS {@code calculate-interest} inserts the system transactions directly into
 * {@code transaction}, so no reload is needed; this step verifies the invariant the legacy reload established:
 * every record of the latest system-transaction file is present in {@code transaction} with the same amount, and
 * {@code transaction} holds at least as many rows as the latest backup. A mismatch → return code 12.
 */
@Component
public class CombineTransactionsJob implements CardDemoJob {

    static final int BATCH = 1000;

    private final JdbcTemplate jdbc;
    private final ObjectStore store;

    public CombineTransactionsJob(JdbcTemplate jdbc, ObjectStore store) {
        this.jdbc = jdbc;
        this.store = store;
    }

    @Override
    public String name() {
        return "combine-transactions";
    }

    @Override
    public JobOutcome run(JobParams p) {
        Map<String, Object> counts = new LinkedHashMap<>();
        List<String> problems = new ArrayList<>();

        Optional<String> systranKey = p.get("systranKey")
                .or(() -> store.latest(S3Keys.systemTransactionsPrefix() + p.businessDate() + "/"))
                .or(() -> store.latest(S3Keys.systemTransactionsPrefix()));
        Optional<String> backupKey = p.get("backupKey").or(() -> store.latest(S3Keys.backupPrefix("transaction")));
        if (systranKey.isEmpty() || backupKey.isEmpty()) {
            throw new JobFailure(ReturnCode.INPUT_ERROR, "combine-transactions needs the latest system-transaction"
                    + " file and transaction backup (SYSTRAN(0) / TRANSACT.BKUP(0)); found systran=" + systranKey
                    + " backup=" + backupKey);
        }
        int systran = 0;
        int missing = 0;
        String content = new String(store.get(systranKey.get()), StandardCharsets.US_ASCII);
        for (String line : content.split("\n")) {
            if (line.isBlank()) {
                continue;
            }
            TransactionRecord r = TransactionRecord.parse(line);
            systran++;
            List<BigDecimal> amts = jdbc.queryForList("SELECT amt FROM transaction WHERE tran_id = ?",
                    BigDecimal.class, r.tranId());
            if (amts.isEmpty() || amts.get(0).compareTo(r.amt()) != 0) {
                missing++;
                if (problems.size() < 20) {
                    problems.add("system transaction " + r.tranId() + " missing or amount differs");
                }
            }
        }
        counts.put("systemTransactionsFile", store.uri(systranKey.get()));
        counts.put("systemTransactions", systran);
        counts.put("systemTransactionsMissing", missing);

        Integer tableRows = jdbc.queryForObject("SELECT count(*) FROM transaction", Integer.class);
        counts.put("transactionRows", tableRows);
        long[] backup = checkBackup(backupKey.get(), problems);
        counts.put("backupFile", store.uri(backupKey.get()));
        counts.put("backupRows", backup[0]);
        counts.put("backupRowsMissing", backup[1]);
        if (!problems.isEmpty()) {
            throw new JobFailureWithCounts(ReturnCode.DATA_ERROR, String.join("; ", problems), counts, null);
        }
        return JobOutcome.ok(counts);
    }

    /**
     * Streams the backup CSV (tran_id is the first column) and checks that every backed-up transaction is still in
     * the table, in batches of {@value #BATCH}. Returns {rows, missing}.
     */
    private long[] checkBackup(String key, List<String> problems) {
        long[] result = {0, 0};
        List<String> ids = new ArrayList<>(BATCH);
        try (Reader in = new BufferedReader(new InputStreamReader(new GZIPInputStream(store.open(key)),
                StandardCharsets.UTF_8))) {
            StringBuilder first = new StringBuilder();
            boolean quoted = false;
            boolean header = true;
            int field = 0;
            for (int c = in.read(); c >= 0; c = in.read()) {
                if (c == '"') {
                    quoted = !quoted;
                } else if (c == ',' && !quoted) {
                    field++;
                } else if (c == '\n' && !quoted) {
                    if (!header) {
                        result[0]++;
                        ids.add(first.toString());
                        if (ids.size() == BATCH) {
                            result[1] += missing(ids, problems);
                        }
                    }
                    header = false;
                    first.setLength(0);
                    field = 0;
                } else if (field == 0) {
                    first.append((char) c);
                }
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        result[1] += missing(ids, problems);
        return result;
    }

    private int missing(List<String> ids, List<String> problems) {
        if (ids.isEmpty()) {
            return 0;
        }
        Set<String> present = new HashSet<>(jdbc.queryForList(
                "SELECT tran_id FROM transaction WHERE tran_id IN (" + String.join(",", Collections.nCopies(ids.size(), "?"))
                        + ")", String.class, ids.toArray()));
        int missing = 0;
        for (String id : ids) {
            if (!present.contains(id)) {
                missing++;
                if (problems.size() < 20) {
                    problems.add("backed-up transaction " + id + " missing from transaction");
                }
            }
        }
        ids.clear();
        return missing;
    }
}
