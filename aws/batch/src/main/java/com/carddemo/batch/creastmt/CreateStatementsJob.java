package com.carddemo.batch.creastmt;

import com.carddemo.batch.core.CardDemoJob;
import com.carddemo.batch.core.JobOutcome;
import com.carddemo.batch.core.JobParams;
import com.carddemo.batch.core.ReturnCode;
import com.carddemo.batch.storage.ObjectStore;
import com.carddemo.batch.storage.S3Keys;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

/**
 * {@code CREASTMT.JCL} → {@code CBSTM03A} (formatting) + {@code CBSTM03B} (file access). The SORT of
 * {@code TRANSACT} by card/tran-id into {@code TRXFL} becomes an ordered query; {@code CBSTM03B}'s
 * {@code O}/{@code C}/{@code R}/{@code K} operations become the queries below. One statement per
 * {@code card_xref} row in card-number order → {@code statements/<businessDate>/<runId>/statement.{txt,html}}.
 * The legacy in-memory table limit (51 cards × 10 transactions) is not reproduced.
 */
@Component
public class CreateStatementsJob implements CardDemoJob {

    private static final Logger log = LoggerFactory.getLogger(CreateStatementsJob.class);

    private final JdbcTemplate jdbc;
    private final ObjectStore store;

    public CreateStatementsJob(JdbcTemplate jdbc, ObjectStore store) {
        this.jdbc = jdbc;
        this.store = store;
    }

    @Override
    public String name() {
        return "create-statements";
    }

    record Xref(String cardNum, int custId, long acctId) {
    }

    @Override
    public JobOutcome run(JobParams p) {
        Map<String, List<StatementWriter.Line>> byCard = new HashMap<>();
        jdbc.query("SELECT card_num, tran_id, description, amt FROM transaction ORDER BY card_num, tran_id",
                rs -> {
                    byCard.computeIfAbsent(rs.getString(1), k -> new ArrayList<>()).add(
                            new StatementWriter.Line(rs.getString(2), rs.getString(3), rs.getBigDecimal(4)));
                });
        List<Xref> xrefs = jdbc.query("SELECT card_num, cust_id, acct_id FROM card_xref ORDER BY card_num",
                (rs, i) -> new Xref(rs.getString(1), rs.getInt(2), rs.getLong(3)));

        StatementWriter writer = new StatementWriter();
        int statements = 0;
        int transactions = 0;
        List<String> skipped = new ArrayList<>();
        for (Xref x : xrefs) {
            List<StatementWriter.Customer> customers = jdbc.query("""
                    SELECT first_name, middle_name, last_name, addr_line_1, addr_line_2, addr_line_3, addr_state_cd,
                           addr_country_cd, addr_zip, fico_credit_score FROM customer WHERE cust_id = ?
                    """, (rs, i) -> new StatementWriter.Customer(rs.getString(1), rs.getString(2), rs.getString(3),
                    rs.getString(4), rs.getString(5), rs.getString(6), rs.getString(7), rs.getString(8),
                    rs.getString(9), (Integer) rs.getObject(10)), x.custId());
            List<BigDecimal> balances = jdbc.queryForList("SELECT curr_bal FROM account WHERE acct_id = ?",
                    BigDecimal.class, x.acctId());
            if (customers.isEmpty() || balances.isEmpty()) {
                skipped.add(x.cardNum());
                log.warn("Card {}: customer {} or account {} not found, statement skipped", x.cardNum(), x.custId(),
                        x.acctId());
                continue;
            }
            List<StatementWriter.Line> lines = byCard.getOrDefault(x.cardNum(), List.of());
            writer.statement(x.acctId(), balances.get(0), customers.get(0), lines);
            statements++;
            transactions += lines.size();
        }
        String txtKey = S3Keys.statement(p.businessDate(), p.runId(), "txt");
        String htmlKey = S3Keys.statement(p.businessDate(), p.runId(), "html");
        store.put(txtKey, writer.text().getBytes(StandardCharsets.UTF_8), "text/plain");
        store.put(htmlKey, writer.html().getBytes(StandardCharsets.UTF_8), "text/html");

        Map<String, Object> counts = new LinkedHashMap<>();
        counts.put("statements", statements);
        counts.put("transactions", transactions);
        counts.put("skipped", skipped.size());
        counts.put("text", store.uri(txtKey));
        counts.put("html", store.uri(htmlKey));
        return skipped.isEmpty() ? JobOutcome.ok(counts)
                : JobOutcome.of(ReturnCode.WARNING, counts, "Statements skipped for cards " + skipped);
    }
}
