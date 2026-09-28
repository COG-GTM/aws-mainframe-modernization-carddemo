package com.carddemo.batch.intcalc;

import com.carddemo.batch.core.CardDemoJob;
import com.carddemo.batch.core.JobFailure;
import com.carddemo.batch.core.JobOutcome;
import com.carddemo.batch.core.JobParams;
import com.carddemo.batch.core.ReturnCode;
import com.carddemo.batch.record.Cobol;
import com.carddemo.batch.record.Fixed;
import com.carddemo.batch.record.TransactionRecord;
import com.carddemo.batch.record.Zoned;
import com.carddemo.batch.storage.ObjectStore;
import com.carddemo.batch.storage.S3Keys;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.sql.Timestamp;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionTemplate;

/**
 * {@code INTCALC.jcl} → {@code CBACT04C PARM='2022071800'}: monthly interest per account / type / category.
 *
 * <p>Walks {@code tran_cat_balance} in VSAM key order ({@code acct_id, type_cd, cat_cd}). For each row the
 * annual rate comes from {@code disclosure_group(account.group_id, type_cd, cat_cd)}, falling back to group
 * {@code DEFAULT}; a non-zero rate yields {@code monthly = balance * rate / 1200} (truncated to cents, no
 * {@code ROUNDED}) and an interest {@code transaction}. Per account: {@code curr_bal += total interest},
 * {@code curr_cyc_credit = curr_cyc_debit = 0}. One DB transaction per account.
 *
 * <p>{@code 1400-COMPUTE-FEES} is "To be implemented" in the COBOL source and is therefore not implemented.
 */
@Component
public class CalculateInterestJob implements CardDemoJob {

    public static final String NAME = "calculate-interest";
    private static final Logger log = LoggerFactory.getLogger(CalculateInterestJob.class);
    private static final BigDecimal TWELVE_HUNDRED = BigDecimal.valueOf(1200);
    private static final DateTimeFormatter PARM_DATE = DateTimeFormatter.ofPattern("yyyyMMdd");

    private final JdbcTemplate jdbc;
    private final TransactionTemplate tx;
    private final ObjectStore store;
    private final Clock clock;

    public CalculateInterestJob(JdbcTemplate jdbc, TransactionTemplate tx, ObjectStore store,
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
        String parm = p.get("parmDate").orElse(p.businessDate().format(PARM_DATE) + "00");
        if (parm.length() != 10) {
            throw new JobFailure(ReturnCode.INPUT_ERROR, "--parmDate must be 10 characters (yyyyMMddNN): " + parm);
        }
        LocalDateTime now = LocalDateTime.now(clock).truncatedTo(ChronoUnit.MICROS);

        List<CatBal> balances = jdbc.query("""
                SELECT acct_id, type_cd, cat_cd, balance FROM tran_cat_balance
                 ORDER BY acct_id, type_cd COLLATE "C", cat_cd
                """, (rs, i) -> new CatBal(rs.getLong(1), rs.getString(2), rs.getInt(3), rs.getBigDecimal(4)));

        int[] suffix = {0};
        int accounts = 0;
        int skipped = 0;
        int interestTransactions = 0;
        BigDecimal totalInterest = BigDecimal.ZERO;
        int i = 0;
        while (i < balances.size()) {
            long acctId = balances.get(i).acctId();
            int j = i;
            while (j < balances.size() && balances.get(j).acctId() == acctId) {
                j++;
            }
            List<CatBal> group = balances.subList(i, j);
            i = j;
            AccountResult result = tx.execute(status -> processAccount(acctId, group, parm, suffix, now));
            accounts++;
            if (result.alreadyApplied()) {
                skipped++;
            } else {
                interestTransactions += result.transactions();
                totalInterest = totalInterest.add(result.interest());
            }
        }

        List<String> systran = jdbc.query("""
                SELECT tran_id, type_cd, cat_cd, source, description, amt, merchant_id, merchant_name,
                       merchant_city, merchant_zip, card_num, orig_ts, proc_ts
                  FROM transaction
                 WHERE tran_id LIKE ? AND type_cd = '01' AND cat_cd = 5 AND source = 'System'
                 ORDER BY tran_id
                """, (rs, n) -> new TransactionRecord(rs.getString(1), rs.getString(2), rs.getInt(3),
                        rs.getString(4), rs.getString(5), rs.getBigDecimal(6), (Integer) rs.getObject(7),
                        rs.getString(8), rs.getString(9), rs.getString(10), rs.getString(11),
                        rs.getObject(12, LocalDateTime.class), rs.getObject(13, LocalDateTime.class))
                        .format(Fixed.DB2_TS), parm + "%");
        String key = S3Keys.systemTransactions(p.businessDate(), p.runId());
        store.put(key, (String.join("\n", systran) + (systran.isEmpty() ? "" : "\n"))
                .getBytes(StandardCharsets.US_ASCII), "text/plain");

        Map<String, Object> counts = new LinkedHashMap<>();
        counts.put("tranCatBalanceRecords", balances.size());
        counts.put("accounts", accounts);
        counts.put("accountsAlreadyApplied", skipped);
        counts.put("interestTransactions", interestTransactions);
        counts.put("totalInterest", totalInterest.toPlainString());
        counts.put("systemTransactionsFile", systran.size());
        return JobOutcome.ok(counts);
    }

    private AccountResult processAccount(long acctId, List<CatBal> group, String parm, int[] suffix,
            LocalDateTime now) {
        // 1100-GET-ACCT-DATA (locked: online bill payment may update the same row)
        List<Acct> accts = jdbc.query("SELECT group_id FROM account WHERE acct_id = ? FOR UPDATE",
                (rs, n) -> new Acct(rs.getString(1)), acctId);
        if (accts.isEmpty()) {
            throw new JobFailure(ReturnCode.DATA_ERROR, "ACCOUNT NOT FOUND: " + acctId);
        }
        String groupId = Optional.ofNullable(accts.get(0).groupId()).orElse("");
        // 1110-GET-XREF-DATA (CXACAIX alternate key: first card of the account)
        List<String> cards = jdbc.queryForList(
                "SELECT card_num FROM card_xref WHERE acct_id = ? ORDER BY card_num LIMIT 1", String.class, acctId);
        if (cards.isEmpty()) {
            throw new JobFailure(ReturnCode.DATA_ERROR, "ACCOUNT NOT FOUND IN XREF: " + acctId);
        }
        String cardNum = cards.get(0);

        List<TransactionRecord> interest = new ArrayList<>();
        BigDecimal total = BigDecimal.ZERO;
        for (CatBal cb : group) {
            BigDecimal rate = interestRate(groupId, cb.typeCd(), cb.catCd());
            if (rate.signum() != 0) {
                BigDecimal monthly = monthlyInterest(cb.balance(), rate);
                total = Cobol.fit(total.add(monthly), 9, 2);
                suffix[0]++;
                interest.add(new TransactionRecord(
                        parm + Zoned.formatUnsigned(suffix[0], 6), "01", 5, "System",
                        "Int. for a/c " + Zoned.formatUnsigned(acctId, 11), monthly, 0, null, null, null,
                        cardNum, now, now));
            }
        }
        if (!interest.isEmpty()) {
            Integer exists = jdbc.queryForObject("SELECT count(*) FROM transaction WHERE tran_id = ?",
                    Integer.class, interest.get(0).tranId());
            if (exists != null && exists > 0) {
                log.info("Interest for account {} already applied ({}), skipping", acctId,
                        interest.get(0).tranId());
                return new AccountResult(true, 0, BigDecimal.ZERO);
            }
        }
        for (TransactionRecord t : interest) {
            jdbc.update("""
                    INSERT INTO transaction (tran_id, type_cd, cat_cd, source, description, amt, merchant_id,
                           merchant_name, merchant_city, merchant_zip, card_num, orig_ts, proc_ts)
                    VALUES (?, ?, ?, ?, ?, ?, ?, NULL, NULL, NULL, ?, ?, ?)
                    """, t.tranId(), t.typeCd(), t.catCd(), t.source(), t.description(), t.amt(), t.merchantId(),
                    t.cardNum(), Timestamp.valueOf(t.origTs()), Timestamp.valueOf(t.procTs()));
        }
        // 1050-UPDATE-ACCOUNT
        jdbc.update("""
                UPDATE account SET curr_bal = curr_bal + ?, curr_cyc_credit = 0, curr_cyc_debit = 0,
                       version = version + 1
                 WHERE acct_id = ?
                """, total, acctId);
        return new AccountResult(false, interest.size(), total);
    }

    /** {@code 1200-GET-INTEREST-RATE} with the {@code 1200-A-GET-DEFAULT-INT-RATE} fallback. */
    private BigDecimal interestRate(String groupId, String typeCd, int catCd) {
        String sql = "SELECT int_rate FROM disclosure_group WHERE acct_group_id = ? AND type_cd = ? AND cat_cd = ?";
        List<BigDecimal> rates = jdbc.queryForList(sql, BigDecimal.class, groupId, typeCd, catCd);
        if (rates.isEmpty()) {
            rates = jdbc.queryForList(sql, BigDecimal.class, "DEFAULT", typeCd, catCd);
        }
        if (rates.isEmpty()) {
            throw new JobFailure(ReturnCode.DATA_ERROR,
                    "ERROR READING DEFAULT DISCLOSURE GROUP for type " + typeCd + " category " + catCd);
        }
        return rates.get(0);
    }

    /** {@code COMPUTE WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200} into {@code S9(09)V99}. */
    public static BigDecimal monthlyInterest(BigDecimal balance, BigDecimal annualRate) {
        BigDecimal exact = balance.multiply(annualRate).divide(TWELVE_HUNDRED, 10, RoundingMode.DOWN);
        return Cobol.fit(exact, 9, 2);
    }

    record CatBal(long acctId, String typeCd, int catCd, BigDecimal balance) {
    }

    record Acct(String groupId) {
    }

    record AccountResult(boolean alreadyApplied, int transactions, BigDecimal interest) {
    }
}
