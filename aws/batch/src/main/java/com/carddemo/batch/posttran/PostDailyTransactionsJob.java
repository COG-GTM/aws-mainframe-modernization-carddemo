package com.carddemo.batch.posttran;

import com.carddemo.batch.core.CardDemoJob;
import com.carddemo.batch.core.JobFailure;
import com.carddemo.batch.core.JobFailureWithCounts;
import com.carddemo.batch.core.JobOutcome;
import com.carddemo.batch.core.JobParams;
import com.carddemo.batch.core.ReturnCode;
import com.carddemo.batch.record.Cobol;
import com.carddemo.batch.record.Fixed;
import com.carddemo.batch.record.TransactionRecord;
import com.carddemo.batch.storage.ObjectStore;
import com.carddemo.batch.storage.S3Keys;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.sql.Timestamp;
import java.time.Clock;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionTemplate;

/**
 * {@code POSTTRAN.jcl} → {@code CBTRN02C}: validates and posts the daily transaction file.
 *
 * <ol>
 *   <li>Stage {@code input/dalytran/<businessDate>/dalytran.txt} into {@code daily_transaction} (once per
 *       {@code runId}).</li>
 *   <li>For each unprocessed row in {@code load_seq} order, in one DB transaction: validate (100/101/102/103),
 *       then either mark it rejected or update {@code tran_cat_balance}, {@code account} and insert
 *       {@code transaction}, and mark it posted.</li>
 *   <li>Write all rejects of the run to {@code output/dalyrejs/<businessDate>/<runId>.txt}
 *       (350-byte record + 4-digit reason + 76-byte description); return code 4 if any.</li>
 * </ol>
 */
@Component
public class PostDailyTransactionsJob implements CardDemoJob {

    public static final String NAME = "post-daily-transactions";
    private static final Logger log = LoggerFactory.getLogger(PostDailyTransactionsJob.class);
    private static final int STAGE_BATCH = 1000;

    private final JdbcTemplate jdbc;
    private final TransactionTemplate tx;
    private final ObjectStore store;
    private final Clock clock;

    public PostDailyTransactionsJob(JdbcTemplate jdbc, TransactionTemplate tx, ObjectStore store,
            Optional<Clock> clock) {
        this.jdbc = jdbc;
        this.tx = tx;
        this.store = store;
        this.clock = clock.orElse(Clock.systemUTC());
    }

    @Override
    public String name() {
        return NAME;
    }

    @Override
    public JobOutcome run(JobParams p) {
        String inputKey = p.get("inputKey").orElse(S3Keys.dailyTransactions(p.businessDate()));
        int staged = stage(p.runId(), inputKey);
        LocalDateTime procTs = LocalDateTime.now(clock).truncatedTo(ChronoUnit.MICROS);

        List<Staged> pending = jdbc.query("""
                SELECT load_seq, tran_id, type_cd, cat_cd, source, description, amt, merchant_id, merchant_name,
                       merchant_city, merchant_zip, card_num, orig_ts, proc_ts
                  FROM daily_transaction WHERE run_id = ? AND post_status IS NULL ORDER BY load_seq
                """, STAGED, p.runId());
        log.info("{} staged rows, {} pending for runId {}", staged, pending.size(), p.runId());

        int postedNow = 0;
        int rejectedNow = 0;
        for (Staged row : pending) {
            RejectReason reason;
            try {
                reason = tx.execute(status -> postOne(p.runId(), row, procTs));
            } catch (DuplicateKeyException e) {
                throw new JobFailureWithCounts(ReturnCode.DATA_ERROR,
                        "Duplicate transaction id " + row.rec().tranId() + " (load_seq " + row.loadSeq() + ")",
                        counts(p.runId()), e);
            }
            if (reason == null) {
                postedNow++;
            } else {
                rejectedNow++;
            }
        }

        List<String> rejects = rejectRecords(p.runId());
        if (!rejects.isEmpty()) {
            String key = S3Keys.dailyRejects(p.businessDate(), p.runId());
            store.put(key, (String.join("\n", rejects) + "\n").getBytes(StandardCharsets.US_ASCII), "text/plain");
            log.info("{} rejects written to {}", rejects.size(), store.uri(key));
        }
        Map<String, Object> counts = counts(p.runId());
        counts.put("postedThisAttempt", postedNow);
        counts.put("rejectedThisAttempt", rejectedNow);
        log.info("TRANSACTIONS PROCESSED :{} TRANSACTIONS REJECTED  :{}", counts.get("processed"),
                counts.get("rejected"));
        return rejects.isEmpty()
                ? JobOutcome.ok(counts)
                : JobOutcome.of(ReturnCode.WARNING, counts, rejects.size() + " transactions rejected");
    }

    /** Stages the input file unless rows already exist for this runId (restart never re-stages). */
    int stage(String runId, String inputKey) {
        Integer existing = jdbc.queryForObject("SELECT count(*) FROM daily_transaction WHERE run_id = ?",
                Integer.class, runId);
        if (existing != null && existing > 0) {
            return existing;
        }
        int staged = tx.execute(status -> {
            List<Object[]> batch = new ArrayList<>(STAGE_BATCH);
            int seq = 0;
            int lineNo = 0;
            try (BufferedReader in = new BufferedReader(
                    new InputStreamReader(store.open(inputKey), StandardCharsets.US_ASCII))) {
                for (String line = in.readLine(); line != null; line = in.readLine()) {
                    lineNo++;
                    if (line.isEmpty()) {
                        continue;
                    }
                    TransactionRecord r;
                    try {
                        r = TransactionRecord.parse(line);
                    } catch (RuntimeException e) {
                        throw new JobFailure(ReturnCode.INPUT_ERROR,
                                "Invalid daily transaction record at line " + lineNo + ": " + e.getMessage(), e);
                    }
                    seq++;
                    batch.add(new Object[] {runId, seq, r.tranId(), r.typeCd(), r.catCd(), r.source(),
                        r.description(), r.amt(), r.merchantId(), r.merchantName(), r.merchantCity(),
                        r.merchantZip(), r.cardNum(), ts(r.origTs()), ts(r.procTs())});
                    if (batch.size() == STAGE_BATCH) {
                        insertStaged(batch);
                    }
                }
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
            insertStaged(batch);
            return seq;
        });
        log.info("Staged {} records from {}", staged, store.uri(inputKey));
        return staged;
    }

    private void insertStaged(List<Object[]> batch) {
        if (batch.isEmpty()) {
            return;
        }
        jdbc.batchUpdate("""
                INSERT INTO daily_transaction (run_id, load_seq, tran_id, type_cd, cat_cd, source, description,
                       amt, merchant_id, merchant_name, merchant_city, merchant_zip, card_num, orig_ts, proc_ts)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """, batch);
        batch.clear();
    }

    /** One input record = one DB transaction. Returns the reject reason, or {@code null} when posted. */
    private RejectReason postOne(String runId, Staged row, LocalDateTime procTs) {
        TransactionRecord r = row.rec();
        RejectReason reason = validateAndPost(r, procTs);
        jdbc.update("UPDATE daily_transaction SET post_status = ?, reject_reason = ? WHERE run_id = ? AND load_seq = ?",
                reason == null ? "P" : "R", reason == null ? null : reason.code(), runId, row.loadSeq());
        return reason;
    }

    private RejectReason validateAndPost(TransactionRecord r, LocalDateTime procTs) {
        // 1500-A-LOOKUP-XREF
        List<Long> acctIds = jdbc.queryForList("SELECT acct_id FROM card_xref WHERE card_num = ?", Long.class,
                r.cardNum());
        if (acctIds.isEmpty()) {
            return RejectReason.INVALID_CARD;
        }
        long acctId = acctIds.get(0);
        // 1500-B-LOOKUP-ACCT (row lock: concurrent online bill payment updates the same row)
        List<Acct> accts = jdbc.query("""
                SELECT credit_limit, curr_cyc_credit, curr_cyc_debit, expiration_date
                  FROM account WHERE acct_id = ? FOR UPDATE
                """, (rs, i) -> new Acct(rs.getBigDecimal(1), rs.getBigDecimal(2), rs.getBigDecimal(3),
                        rs.getObject(4, LocalDate.class)), acctId);
        if (accts.isEmpty()) {
            return RejectReason.ACCOUNT_NOT_FOUND;
        }
        Acct a = accts.get(0);
        RejectReason reason = validate(a, r);
        if (reason != null) {
            return reason;
        }
        // 2800-UPDATE-ACCOUNT-REC (done first so a 109 leaves nothing else changed)
        boolean credit = r.amt().signum() >= 0;
        int updated = jdbc.update("""
                UPDATE account SET curr_bal = curr_bal + ?,
                       curr_cyc_credit = curr_cyc_credit + ?, curr_cyc_debit = curr_cyc_debit + ?,
                       version = version + 1
                 WHERE acct_id = ?
                """, r.amt(), credit ? r.amt() : BigDecimal.ZERO, credit ? BigDecimal.ZERO : r.amt(), acctId);
        if (updated != 1) {
            return RejectReason.ACCOUNT_REWRITE_FAILED;
        }
        // 2700-UPDATE-TCATBAL
        jdbc.update("""
                INSERT INTO tran_cat_balance (acct_id, type_cd, cat_cd, balance) VALUES (?, ?, ?, ?)
                ON CONFLICT (acct_id, type_cd, cat_cd)
                DO UPDATE SET balance = tran_cat_balance.balance + EXCLUDED.balance,
                              version = tran_cat_balance.version + 1
                """, acctId, r.typeCd(), r.catCd(), r.amt());
        // 2900-WRITE-TRANSACTION-FILE
        jdbc.update("""
                INSERT INTO transaction (tran_id, type_cd, cat_cd, source, description, amt, merchant_id,
                       merchant_name, merchant_city, merchant_zip, card_num, orig_ts, proc_ts)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """, r.tranId(), r.typeCd(), r.catCd(), r.source(), r.description(), r.amt(), r.merchantId(),
                r.merchantName(), r.merchantCity(), r.merchantZip(), r.cardNum(), ts(r.origTs()), ts(procTs));
        return null;
    }

    /**
     * {@code 1500-B-LOOKUP-ACCT} checks, in COBOL order (a later failing check overwrites the earlier reason).
     * {@code WS-TEMP-BAL} is {@code S9(09)V99}; the expiry test is the COBOL alphanumeric compare
     * {@code ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)} (blank expiry sorts low → reject).
     */
    static RejectReason validate(Acct a, TransactionRecord r) {
        RejectReason reason = null;
        BigDecimal tempBal = Cobol.fit(a.cycCredit().subtract(a.cycDebit()).add(r.amt()), 9, 2);
        if (a.creditLimit().compareTo(tempBal) < 0) {
            reason = RejectReason.OVERLIMIT;
        }
        LocalDate origDate = r.origTs() == null ? null : r.origTs().toLocalDate();
        if (origDate != null && (a.expirationDate() == null || a.expirationDate().isBefore(origDate))) {
            reason = RejectReason.EXPIRED_ACCOUNT;
        }
        return reason;
    }

    private List<String> rejectRecords(String runId) {
        return jdbc.query("""
                SELECT load_seq, tran_id, type_cd, cat_cd, source, description, amt, merchant_id, merchant_name,
                       merchant_city, merchant_zip, card_num, orig_ts, proc_ts, reject_reason
                  FROM daily_transaction WHERE run_id = ? AND post_status = 'R' ORDER BY load_seq
                """, (rs, i) -> {
                    Staged s = STAGED.mapRow(rs, i);
                    RejectReason reason = RejectReason.of(rs.getInt("reject_reason"));
                    return s.rec().format(Fixed.ISO_SPACE_TS)
                            + String.format("%04d", reason.code())
                            + Fixed.pad(reason.description(), 76);
                }, runId);
    }

    private Map<String, Object> counts(String runId) {
        Map<String, Object> counts = new LinkedHashMap<>();
        jdbc.query("""
                SELECT count(*) AS processed,
                       count(*) FILTER (WHERE post_status = 'P') AS posted,
                       count(*) FILTER (WHERE post_status = 'R') AS rejected
                  FROM daily_transaction WHERE run_id = ?
                """, rs -> {
                    counts.put("processed", rs.getInt("processed"));
                    counts.put("posted", rs.getInt("posted"));
                    counts.put("rejected", rs.getInt("rejected"));
                }, runId);
        return counts;
    }

    private static Timestamp ts(LocalDateTime t) {
        return t == null ? null : Timestamp.valueOf(t);
    }

    record Acct(BigDecimal creditLimit, BigDecimal cycCredit, BigDecimal cycDebit, LocalDate expirationDate) {
    }

    record Staged(int loadSeq, TransactionRecord rec) {
    }

    private static final RowMapper<Staged> STAGED = (rs, i) -> new Staged(rs.getInt("load_seq"),
            new TransactionRecord(
                    rs.getString("tran_id"),
                    rs.getString("type_cd"),
                    rs.getInt("cat_cd"),
                    rs.getString("source"),
                    rs.getString("description"),
                    rs.getBigDecimal("amt"),
                    (Integer) rs.getObject("merchant_id"),
                    rs.getString("merchant_name"),
                    rs.getString("merchant_city"),
                    rs.getString("merchant_zip"),
                    rs.getString("card_num"),
                    rs.getObject("orig_ts", LocalDateTime.class),
                    rs.getObject("proc_ts", LocalDateTime.class)));
}
