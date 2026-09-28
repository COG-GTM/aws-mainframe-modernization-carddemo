package com.carddemo.batch.it;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.core.ReturnCode;
import com.carddemo.batch.record.AccountRecord;
import com.carddemo.batch.record.Fixed;
import com.carddemo.batch.record.TranCatBalRecord;
import com.carddemo.batch.record.TransactionRecord;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;

/**
 * Compares the Java jobs with outputs of the real COBOL programs ({@code CBTRN02C}, {@code CBACT04C}) compiled
 * with GnuCOBOL on the same {@code app/data/ASCII} input ({@code golden/generate-golden.sh}).
 */
class GoldenPosttranIntcalcIT extends AbstractBatchIT {

    private final ObjectMapper mapper = new ObjectMapper();

    @Test
    void posttranMatchesCbtrn02c() throws IOException {
        int rc = run("post-daily-transactions", "post-1");

        assertThat(rc).isEqualTo(Integer.parseInt(golden("posttran/returncode.txt").get(0).trim()))
                .isEqualTo(ReturnCode.WARNING);
        JsonNode result = mapper.readTree(read("runs/post-1/post-daily-transactions.json"));
        assertThat(result.get("returnCode").asInt()).isEqualTo(4);
        assertThat(result.get("status").asText()).isEqualTo("COMPLETED");
        List<String> counts = golden("posttran/counts.txt");
        assertThat(result.at("/counts/processed").asInt()).isEqualTo(Integer.parseInt(counts.get(0).split(":")[1]));
        assertThat(result.at("/counts/rejected").asInt()).isEqualTo(Integer.parseInt(counts.get(1).split(":")[1]));

        assertThat(lines(read("output/dalyrejs/2022-07-18/post-1.txt"))).containsExactlyElementsOf(
                golden("posttran/dalyrejs.txt"));
        assertThat(accounts()).containsExactlyElementsOf(golden("posttran/acctdata.txt"));
        assertThat(tranCatBalances()).containsExactlyElementsOf(goldenTcatbal());
        assertThat(transactions()).containsExactlyElementsOf(golden("posttran/transact.txt"));
        assertThat(jdbc.queryForList("SELECT DISTINCT proc_ts FROM transaction", LocalDateTime.class))
                .containsExactly(LocalDateTime.of(2022, 7, 18, 10, 0));
    }

    @Test
    void posttranRerunOfCompletedRunIdIsNoOp() throws IOException {
        assertThat(run("post-daily-transactions", "post-1")).isEqualTo(4);
        List<String> before = accounts();
        assertThat(run("post-daily-transactions", "post-1")).isEqualTo(4);
        assertThat(accounts()).isEqualTo(before);
        assertThat(jdbc.queryForObject("SELECT count(*) FROM daily_transaction", Integer.class)).isEqualTo(300);
        assertThat(read("runs/post-1/post-daily-transactions.json")).contains("already completed");
    }

    /**
     * A duplicate {@code tran_id} fails the run with RC 12 after the preceding records were committed (one DB
     * transaction per record); after removing the conflict, a restart with the same runId processes only the
     * remaining rows and ends in exactly the COBOL end state.
     */
    @Test
    void posttranRestartAfterFailureAppliesEachRecordOnce() throws IOException {
        List<String> daily = lines(read("input/dalytran/2022-07-18/dalytran.txt"));
        List<String> posted = golden("posttran/transact.txt").stream().map(l -> l.substring(0, 16)).toList();
        TransactionRecord victim = daily.stream().skip(100).map(TransactionRecord::parse)
                .filter(r -> posted.contains(r.tranId())).findFirst().orElseThrow();
        jdbc.update("INSERT INTO transaction (tran_id, type_cd, cat_cd, amt, card_num) VALUES (?, ?, ?, 0, ?)",
                victim.tranId(), victim.typeCd(), victim.catCd(), victim.cardNum());

        assertThat(run("post-daily-transactions", "post-r")).isEqualTo(ReturnCode.DATA_ERROR);
        assertThat(read("runs/post-r/post-daily-transactions.json")).contains("Duplicate transaction id");
        int done = jdbc.queryForObject(
                "SELECT count(*) FROM daily_transaction WHERE run_id = 'post-r' AND post_status IS NOT NULL",
                Integer.class);
        assertThat(done).isGreaterThan(100).isLessThan(300);

        jdbc.update("DELETE FROM transaction WHERE tran_id = ?", victim.tranId());
        assertThat(run("post-daily-transactions", "post-r")).isEqualTo(ReturnCode.WARNING);
        assertThat(jdbc.queryForObject("SELECT count(*) FROM daily_transaction", Integer.class)).isEqualTo(300);
        assertThat(accounts()).containsExactlyElementsOf(golden("posttran/acctdata.txt"));
        assertThat(tranCatBalances()).containsExactlyElementsOf(goldenTcatbal());
        assertThat(lines(read("output/dalyrejs/2022-07-18/post-r.txt"))).containsExactlyElementsOf(
                golden("posttran/dalyrejs.txt"));
    }

    @Test
    void missingInputFileIsReturnCode8() {
        assertThat(run("post-daily-transactions", "post-x", "--inputKey=input/dalytran/none.txt"))
                .isEqualTo(ReturnCode.INPUT_ERROR);
    }

    /**
     * INTCALC runs on the POSTTRAN result, like the golden harness. {@code CBACT04C} never calls
     * {@code 1050-UPDATE-ACCOUNT} for the last account of {@code TCATBAL} (the EOF branch is unreachable), so the
     * COBOL output leaves that account untouched; the Java job updates it (documented deviation) and the test
     * checks it against the value the COBOL formula gives.
     */
    @Test
    void intcalcMatchesCbact04c() throws IOException {
        assertThat(run("post-daily-transactions", "post-1")).isEqualTo(4);
        List<String> afterPost = accounts();

        int rc = run("calculate-interest", "int-1", "--parmDate=2022071800");
        assertThat(rc).isEqualTo(Integer.parseInt(golden("intcalc/returncode.txt").get(0).trim()));

        List<String> systran = new ArrayList<>();
        for (String l : lines(read("output/systran/2022-07-18/int-1.txt"))) {
            systran.add(l.substring(0, 278) + "ORIG-TS-MASKED            PROC-TS-MASKED            "
                    + l.substring(330));
        }
        assertThat(systran).containsExactlyElementsOf(golden("intcalc/systran.txt")).hasSize(50);

        long lastAcct = jdbc.queryForObject("SELECT max(acct_id) FROM tran_cat_balance", Long.class);
        List<String> expected = golden("intcalc/acctdata.txt");
        List<String> actual = accounts();
        assertThat(actual).hasSameSizeAs(expected);
        for (int i = 0; i < actual.size(); i++) {
            AccountRecord a = AccountRecord.parse(actual.get(i));
            if (a.acctId() == lastAcct) {
                AccountRecord before = AccountRecord.parse(afterPost.get(i));
                java.math.BigDecimal interest = jdbc.queryForObject(
                        "SELECT coalesce(sum(amt), 0) FROM transaction WHERE description = ?",
                        java.math.BigDecimal.class, "Int. for a/c " + String.format("%011d", lastAcct));
                assertThat(a.currBal()).isEqualByComparingTo(before.currBal().add(interest));
                assertThat(a.currCycCredit()).isZero();
                assertThat(a.currCycDebit()).isZero();
                assertThat(expected.get(i)).isEqualTo(afterPost.get(i));
            } else {
                assertThat(actual.get(i)).isEqualTo(expected.get(i));
            }
        }

        assertThat(run("calculate-interest", "int-2", "--parmDate=2022071800")).isEqualTo(ReturnCode.OK);
        assertThat(accounts()).isEqualTo(actual);
        assertThat(read("runs/int-2/calculate-interest.json")).contains("\"accountsAlreadyApplied\" : 50");
    }

    @Test
    void intcalcRerunForSameParmDateNeverResetsCycleTotalsAgain() {
        assertThat(run("post-daily-transactions", "zr-post")).isEqualTo(ReturnCode.WARNING);
        jdbc.update("UPDATE disclosure_group SET int_rate = 0");
        assertThat(run("calculate-interest", "zr-1", "--parmDate=2022071800")).isEqualTo(ReturnCode.OK);
        assertThat(jdbc.queryForObject("SELECT count(*) FROM transaction WHERE source = 'System'", Integer.class))
                .isZero();
        jdbc.update("UPDATE account SET curr_cyc_credit = 50");
        assertThat(run("calculate-interest", "zr-2", "--parmDate=2022071800")).isEqualTo(ReturnCode.OK);
        assertThat(jdbc.queryForObject("SELECT count(*) FROM account WHERE curr_cyc_credit <> 50", Integer.class))
                .isZero();
    }

    @Test
    void intcalcRerunSystranContainsOnlyItsOwnTransactions() throws IOException {
        assertThat(run("post-daily-transactions", "sy-post")).isEqualTo(ReturnCode.WARNING);
        assertThat(run("calculate-interest", "sy-1", "--parmDate=2022071800")).isEqualTo(ReturnCode.OK);
        assertThat(read("output/systran/2022-07-18/sy-1.txt")).isNotEmpty();
        assertThat(run("calculate-interest", "sy-2", "--parmDate=2022071800")).isEqualTo(ReturnCode.OK);
        assertThat(read("output/systran/2022-07-18/sy-2.txt")).isEmpty();
    }

    @Test
    void rejectsKeepTheSubmittedRecordBytes() throws IOException {
        String input = read("input/dalytran/2022-07-18/dalytran.txt");
        String first = input.substring(0, input.indexOf('\n'));
        String record = Fixed.pad(first.substring(0, 262) + "9999999999999999"
                + first.substring(278, 304).replace(' ', '-').replace(':', '.') + first.substring(304), 350);
        put("input/dalytran/2022-07-18/db2.txt", record + "\n");
        assertThat(run("post-daily-transactions", "raw", "--inputKey=input/dalytran/2022-07-18/db2.txt"))
                .isEqualTo(ReturnCode.WARNING);
        assertThat(lines(read("output/dalyrejs/2022-07-18/raw.txt")).get(0)).startsWith(record + "0100");
    }

    @Test
    void completedRunIdReusedForAnotherBusinessDateIsRejected() {
        assertThat(run("post-daily-transactions", "bd")).isEqualTo(ReturnCode.WARNING);
        assertThat(runner.run("--job=post-daily-transactions", "--runId=bd", "--businessDate=2022-07-19"))
                .isEqualTo(ReturnCode.INPUT_ERROR);
    }

    List<String> accounts() {
        return jdbc.query("""
                SELECT acct_id, active_status, curr_bal, credit_limit, cash_credit_limit, open_date, expiration_date,
                       reissue_date, curr_cyc_credit, curr_cyc_debit, addr_zip, group_id FROM account ORDER BY acct_id
                """, (rs, i) -> new AccountRecord(rs.getLong(1), rs.getString(2), rs.getBigDecimal(3),
                rs.getBigDecimal(4), rs.getBigDecimal(5), rs.getObject(6, LocalDate.class),
                rs.getObject(7, LocalDate.class), rs.getObject(8, LocalDate.class), rs.getBigDecimal(9),
                rs.getBigDecimal(10), rs.getString(11), rs.getString(12)).format());
    }

    List<String> tranCatBalances() {
        return jdbc.query("SELECT acct_id, type_cd, cat_cd, balance FROM tran_cat_balance "
                + "ORDER BY acct_id, type_cd COLLATE \"C\", cat_cd",
                (rs, i) -> new TranCatBalRecord(rs.getLong(1), rs.getString(2), rs.getInt(3), rs.getBigDecimal(4))
                        .format().substring(0, TCATBAL_DATA));
    }

    /** The 22-byte {@code FILLER} of {@code CVTRA01Y} is zeros in the ASCII fixture and not stored in Aurora. */
    static final int TCATBAL_DATA = 28;

    static List<String> goldenTcatbal() throws IOException {
        return golden("posttran/tcatbal.txt").stream().map(l -> l.substring(0, TCATBAL_DATA)).toList();
    }

    List<String> transactions() {
        return jdbc.query("""
                SELECT tran_id, type_cd, cat_cd, source, description, amt, merchant_id, merchant_name, merchant_city,
                       merchant_zip, card_num, orig_ts, proc_ts FROM transaction ORDER BY tran_id
                """, (rs, i) -> {
                    String l = new TransactionRecord(rs.getString(1), rs.getString(2), rs.getInt(3), rs.getString(4),
                            rs.getString(5), rs.getBigDecimal(6), (Integer) rs.getObject(7), rs.getString(8),
                            rs.getString(9), rs.getString(10), rs.getString(11),
                            rs.getObject(12, LocalDateTime.class), rs.getObject(13, LocalDateTime.class))
                            .format(Fixed.ISO_SPACE_TS);
                    return l.substring(0, 304) + "PROC-TS-MASKED            " + l.substring(330);
                });
    }
}
