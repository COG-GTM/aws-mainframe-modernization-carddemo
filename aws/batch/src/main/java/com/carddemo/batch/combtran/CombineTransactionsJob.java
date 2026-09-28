package com.carddemo.batch.combtran;

import com.carddemo.batch.core.CardDemoJob;
import com.carddemo.batch.core.JobFailureWithCounts;
import com.carddemo.batch.core.JobOutcome;
import com.carddemo.batch.core.JobParams;
import com.carddemo.batch.core.ReturnCode;
import com.carddemo.batch.record.TransactionRecord;
import com.carddemo.batch.storage.ObjectStore;
import com.carddemo.batch.storage.S3Keys;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
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
        int systran = 0;
        int missing = 0;
        if (systranKey.isPresent()) {
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
        }
        counts.put("systemTransactions", systran);
        counts.put("systemTransactionsMissing", missing);

        Integer tableRows = jdbc.queryForObject("SELECT count(*) FROM transaction", Integer.class);
        counts.put("transactionRows", tableRows);
        Optional<String> backupKey = p.get("backupKey").or(() -> store.latest(S3Keys.backupPrefix("transaction")));
        if (backupKey.isPresent()) {
            long backupRows = csvRows(store.get(backupKey.get()));
            counts.put("backupFile", store.uri(backupKey.get()));
            counts.put("backupRows", backupRows);
            if (tableRows == null || tableRows < backupRows) {
                problems.add("transaction has " + tableRows + " rows, fewer than backup " + backupRows);
            }
        }
        if (!problems.isEmpty()) {
            throw new JobFailureWithCounts(ReturnCode.DATA_ERROR, String.join("; ", problems), counts, null);
        }
        return JobOutcome.ok(counts);
    }

    private static long csvRows(byte[] gz) {
        try (InputStream in = new GZIPInputStream(new ByteArrayInputStream(gz))) {
            String csv = new String(in.readAllBytes(), StandardCharsets.UTF_8);
            long lines = 0;
            boolean quoted = false;
            for (int i = 0; i < csv.length(); i++) {
                char c = csv.charAt(i);
                if (c == '"') {
                    quoted = !quoted;
                } else if (c == '\n' && !quoted) {
                    lines++;
                }
            }
            return Math.max(0, lines - 1);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
