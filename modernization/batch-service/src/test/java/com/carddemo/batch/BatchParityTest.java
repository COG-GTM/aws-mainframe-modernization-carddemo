package com.carddemo.batch;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.common.domain.Account;
import com.carddemo.common.domain.CardXref;
import com.carddemo.common.domain.DailyTransaction;
import com.carddemo.common.testing.ParityTestSupport;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.web.servlet.MockMvc;

/** Parity with CBTRN02C (POSTTRAN), CBACT04C (INTCALC), CBTRN03C (TRANREPT) and CBSTM03A/B (CREASTMT). */
@SpringBootTest
@AutoConfigureMockMvc
class BatchParityTest extends ParityTestSupport {

    @Autowired
    MockMvc mvc;

    @Autowired
    JdbcTemplate jdbc;

    @Autowired
    PostTransactionsService posttran;

    @Autowired
    InterestCalculationService intcalc;

    @Autowired
    TransactionReportService tranrept;

    @Autowired
    StatementService creastmt;

    private void daily(String id, String card, String amount, String origTs) {
        jdbc.update("insert into daily_transaction (tran_id, type_cd, cat_cd, source, description, amount, merchant_id,"
                + " merchant_name, merchant_city, merchant_zip, card_num, orig_ts, proc_ts, status) values"
                + " (?, '01', 1, 'POS TERM', 'Test', ?, 1, 'M', 'C', '1', ?, ?, ?, 'PENDING')",
                id, new BigDecimal(amount), card, LocalDateTime.parse(origTs), LocalDateTime.parse(origTs));
    }

    private BigDecimal col(String sql, Object... args) {
        return jdbc.queryForObject(sql, BigDecimal.class, args);
    }

    @Test
    void posttranPostsValidAndRejectsWithLegacyReasonCodes() {
        daily("D000000000000001", "4111111111111111", "50.00", "2024-01-01T09:00:00");
        daily("D000000000000002", "9999999999999999", "10.00", "2024-01-01T09:00:00");
        daily("D000000000000003", "4333333333333333", "200.00", "2024-01-01T09:00:00");
        daily("D000000000000004", "4333333333333333", "200.00", "2019-06-01T09:00:00");
        daily("D000000000000005", "4222222222222222", "-25.00", "2024-01-01T09:00:00");

        JobResult r = posttran.run();

        assertThat(r.returnCode()).isEqualTo(4);
        assertThat(r.counters()).containsEntry("transactionsProcessed", 5).containsEntry("transactionsRejected", 3);
        List<Map<String, Object>> rejects = jdbc.queryForList(
                "select tran_id, reason_code, reason_desc from daily_transaction_reject order by tran_id");
        assertThat(rejects).extracting(m -> m.get("reason_code")).containsExactly(100, 103, 102);
        assertThat(rejects.get(0).get("reason_desc")).isEqualTo("INVALID CARD NUMBER FOUND");
        assertThat(rejects.get(1).get("reason_desc")).isEqualTo("TRANSACTION RECEIVED AFTER ACCT EXPIRATION");
        assertThat(rejects.get(2).get("reason_desc")).isEqualTo("OVERLIMIT TRANSACTION");

        assertThat(col("select curr_bal from account where acct_id = 1")).isEqualByComparingTo("244.00");
        assertThat(col("select curr_cyc_credit from account where acct_id = 1")).isEqualByComparingTo("50.00");
        assertThat(col("select balance from tran_cat_balance where acct_id = 1 and type_cd = '01' and cat_cd = 1"))
                .isEqualByComparingTo("150.00");
        assertThat(col("select curr_cyc_debit from account where acct_id = 2")).isEqualByComparingTo("-25.00");
        assertThat(col("select curr_bal from account where acct_id = 3")).isEqualByComparingTo("450.00");
        assertThat(jdbc.queryForObject("select count(*) from card_transaction where tran_id like 'D%'", Integer.class))
                .isEqualTo(2);
        assertThat(jdbc.queryForList("select status from daily_transaction order by tran_id", String.class))
                .containsExactly("POSTED", "REJECTED", "REJECTED", "REJECTED", "POSTED");
    }

    @Test
    void posttranReasonCode101WhenAccountMissing() {
        CardXref x = new CardXref();
        x.setCardNum("4999999999999999");
        x.setAcctId(99L);
        DailyTransaction t = new DailyTransaction();
        t.setAmount(BigDecimal.ONE);
        assertThat(PostTransactionsService.validate(Optional.of(x), Optional.<Account>empty(), t).code())
                .isEqualTo(101);
    }

    @Test
    void intcalcAppliesGroupRateDefaultFallbackAndResetsCycle() {
        JobResult r = intcalc.run(LocalDate.of(2024, 1, 31));

        // acct 1 (A000000000): 100*15/1200 = 1.25, 50*25/1200 = 1.0416 -> 1.04 (truncated)
        // acct 2 (no group -> DEFAULT): 200*15/1200 = 2.50; acct 3 (ZEROAPR, 0%): no interest transaction
        assertThat(r.counters()).containsEntry("interestTransactions", 3).containsEntry("accountsUpdated", 3);
        assertThat((BigDecimal) r.counters().get("totalInterest")).isEqualByComparingTo("4.79");
        assertThat(col("select curr_bal from account where acct_id = 1")).isEqualByComparingTo("196.29");
        assertThat(col("select curr_bal from account where acct_id = 2")).isEqualByComparingTo("2.50");
        assertThat(col("select curr_bal from account where acct_id = 3")).isEqualByComparingTo("450.00");
        assertThat(col("select curr_cyc_credit from account where acct_id = 3")).isEqualByComparingTo("0");
        Map<String, Object> first = jdbc.queryForMap("select * from card_transaction where tran_id = '2024-01-31000001'");
        assertThat(first.get("type_cd")).isEqualTo("01");
        assertThat(first.get("cat_cd")).isEqualTo(5);
        assertThat(first.get("source")).isEqualTo("System");
        assertThat(first.get("description")).isEqualTo("Int. for a/c 00000000001");
        assertThat(first.get("card_num")).isEqualTo("4111111111111111");
        assertThat((BigDecimal) first.get("amount")).isEqualByComparingTo("1.25");
    }

    @Test
    void intcalcTruncatesLikeCobolCompute() {
        assertThat(InterestCalculationService.monthlyInterest(new BigDecimal("50.00"), new BigDecimal("25.00")))
                .isEqualByComparingTo("1.04");
        assertThat(InterestCalculationService.monthlyInterest(new BigDecimal("-100.00"), new BigDecimal("13.00")))
                .isEqualByComparingTo("-1.08");
    }

    @Test
    void tranreptFiltersByProcDateAndTotals() {
        JobResult r = tranrept.run(LocalDate.of(2022, 6, 1), LocalDate.of(2022, 6, 5));
        BigDecimal expected = col("select sum(amount) from card_transaction where proc_ts >= '2022-06-01'"
                + " and proc_ts < '2022-06-06'");
        assertThat(r.counters()).containsEntry("transactions", 5);
        assertThat((BigDecimal) r.counters().get("grandTotal")).isEqualByComparingTo(expected);
        assertThat(r.output()).contains("Date Range: 2022-06-01 to 2022-06-05").contains("Grand Total")
                .contains("0000000000000005").doesNotContain("0000000000000006");
    }

    @Test
    void tranreptPageTotalEveryTwentyLines() {
        for (int i = 0; i < 15; i++) {
            jdbc.update("insert into card_transaction (tran_id, type_cd, cat_cd, source, description, amount,"
                    + " merchant_id, merchant_name, merchant_city, merchant_zip, card_num, orig_ts, proc_ts) values"
                    + " (?, '01', 1, 'POS TERM', 'x', 1.00, 1, 'm', 'c', 'z', '4222222222222222',"
                    + " '2022-06-20 10:00:00', '2022-06-20 10:00:00')", String.format("9%015d", i));
        }
        String out = tranrept.run(LocalDate.of(2022, 6, 1), LocalDate.of(2022, 6, 30)).output();
        // 12 fixture rows (card ...111) + 15 rows (card ...222) = 27 lines -> one page break, two account breaks
        assertThat(out.split("Page Total", -1)).hasSize(3);
        assertThat(out.split("Account Total", -1)).hasSize(3);
    }

    @Test
    void corpt00cRequestIsCompletedByBatch() {
        jdbc.update("insert into report_request (report_type, start_date, end_date, requested_by, status, requested_at)"
                + " values ('CUSTOM', '2022-06-01', '2022-06-30', 'USER0001', 'SUBMITTED', current_timestamp)");
        assertThat(tranrept.processSubmittedRequests()).isEqualTo(1);
        assertThat(jdbc.queryForObject("select status from report_request", String.class)).isEqualTo("COMPLETED");
        assertThat(jdbc.queryForObject("select output from report_request", String.class)).contains("Grand Total");
    }

    @Test
    void creastmtOneStatementPerXref() {
        JobResult r = creastmt.run();
        assertThat(r.counters()).containsEntry("statements", 4);
        assertThat(r.output()).contains("START OF STATEMENT").contains("Kessler").contains("Total EXP:");
    }

    @Test
    void batchApiIsAdminOnlyAndRunsJobs() throws Exception {
        mvc.perform(post("/api/v1/batch/jobs/INTCALC").header("Authorization", userToken()))
                .andExpect(status().isForbidden());
        mvc.perform(post("/api/v1/batch/jobs/INTCALC?date=2024-01-31").header("Authorization", adminToken()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.status").value("COMPLETED"))
                .andExpect(jsonPath("$.returnCode").value(0))
                .andExpect(jsonPath("$.counters.interestTransactions").value(3));
        String rec = String.format("%-16s%-2s%04d%-10s%-100s%s%09d%-50s%-50s%-10s%-16s%-26s%-26s%-20s",
                "T000000000000001", "01", 1, "POS TERM", "Coffee", "0000000450{", 1, "Cafe", "Zurich", "8001",
                "4111111111111111", "2024-01-15-10.00.00.000000", "2024-01-15-10.00.00.000000", "");
        mvc.perform(post("/api/v1/batch/daily-transactions").header("Authorization", adminToken())
                        .contentType(MediaType.TEXT_PLAIN).content(rec + "\n"))
                .andExpect(status().isAccepted())
                .andExpect(jsonPath("$.staged").value(1));
        mvc.perform(post("/api/v1/batch/jobs/POSTTRAN").header("Authorization", adminToken()))
                .andExpect(jsonPath("$.returnCode").value(0))
                .andExpect(jsonPath("$.counters.transactionsProcessed").value(1));
        assertThat(col("select amount from card_transaction where tran_id = 'T000000000000001'"))
                .isEqualByComparingTo("45.00");
    }

    @Test
    void intcalcRefusesSecondRunForSameDate() {
        intcalc.run(LocalDate.of(2024, 1, 31));
        JobResult again = intcalc.run(LocalDate.of(2024, 1, 31));
        assertThat(again.returnCode()).isEqualTo(8);
        assertThat(col("select curr_bal from account where acct_id = 1")).isEqualByComparingTo("196.29");
        assertThat(jdbc.queryForObject("select count(*) from card_transaction where tran_id like '2024-01-31%'",
                Integer.class)).isEqualTo(3);
    }
}
