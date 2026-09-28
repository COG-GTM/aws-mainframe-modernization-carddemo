package com.carddemo.batch.tranextr;

import com.carddemo.batch.core.CardDemoJob;
import com.carddemo.batch.core.JobOutcome;
import com.carddemo.batch.core.JobParams;
import com.carddemo.batch.record.Fixed;
import com.carddemo.batch.record.Zoned;
import com.carddemo.batch.storage.ObjectStore;
import com.carddemo.batch.storage.S3Keys;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

/**
 * {@code TRANEXTR.jcl} STEP30/STEP40 ({@code DSNTIAUL} unload of {@code TRANSACTION_TYPE} /
 * {@code TRANSACTION_TYPE_CATEGORY}) → 60-byte {@code CVTRA03Y}/{@code CVTRA04Y} records under
 * {@code refdata/transaction_type/<runId>.txt} and {@code refdata/transaction_category/<runId>.txt}. Those
 * objects are what {@code load-reference-data} picks up; since the VSAM and DB2 tables are a single Aurora table,
 * no separate VSAM refresh is needed. STEP10/20 (IEBGENER backups of the previous extract) are implicit: every
 * extract is a new immutable object.
 */
@Component
public class ExtractTransactionTypesJob implements CardDemoJob {

    private final JdbcTemplate jdbc;
    private final ObjectStore store;

    public ExtractTransactionTypesJob(JdbcTemplate jdbc, ObjectStore store) {
        this.jdbc = jdbc;
        this.store = store;
    }

    @Override
    public String name() {
        return "extract-transaction-types";
    }

    @Override
    public JobOutcome run(JobParams p) {
        List<String> types = jdbc.query(
                "SELECT type_cd, description FROM transaction_type ORDER BY type_cd COLLATE \"C\"",
                (rs, i) -> Fixed.pad(Fixed.pad(rs.getString(1), 2) + rs.getString(2), 60));
        List<String> categories = jdbc.query(
                "SELECT type_cd, cat_cd, description FROM transaction_category ORDER BY type_cd COLLATE \"C\", cat_cd",
                (rs, i) -> Fixed.pad(Fixed.pad(rs.getString(1), 2) + Zoned.formatUnsigned(rs.getInt(2), 4)
                        + rs.getString(3), 60));
        String typeKey = S3Keys.refdata("transaction_type", p.runId());
        String catKey = S3Keys.refdata("transaction_category", p.runId());
        store.put(typeKey, lines(types), "text/plain");
        store.put(catKey, lines(categories), "text/plain");
        Map<String, Object> counts = new LinkedHashMap<>();
        counts.put("transactionTypes", types.size());
        counts.put("transactionCategories", categories.size());
        counts.put("typeOutput", store.uri(typeKey));
        counts.put("categoryOutput", store.uri(catKey));
        return JobOutcome.ok(counts);
    }

    private static byte[] lines(List<String> lines) {
        return (String.join("\n", lines) + (lines.isEmpty() ? "" : "\n")).getBytes(StandardCharsets.US_ASCII);
    }
}
