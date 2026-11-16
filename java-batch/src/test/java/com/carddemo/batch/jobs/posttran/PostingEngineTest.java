package com.carddemo.batch.jobs.posttran;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.io.KsdsFile;
import com.carddemo.batch.model.AccountRecord;
import com.carddemo.batch.model.CardXrefRecord;
import com.carddemo.batch.model.DailyTransactionRecord;
import com.carddemo.batch.model.TranCatBalRecord;
import com.carddemo.batch.model.TranCatBalRecord.TranCatKey;
import com.carddemo.batch.model.TransactionRecord;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class PostingEngineTest {

    static final String CARD = "4859452612877065";
    static final long ACCT = 1L;

    KsdsFile<String, CardXrefRecord> xref;
    KsdsFile<Long, AccountRecord> accounts;
    KsdsFile<TranCatKey, TranCatBalRecord> tcatbal;
    KsdsFile<String, TransactionRecord> transact;
    PostingEngine engine;

    @BeforeEach
    void setUp() {
        xref = new KsdsFile<>(CardXrefRecord.CODEC, CardXrefRecord::xrefCardNum);
        accounts = new KsdsFile<>(AccountRecord.CODEC, AccountRecord::acctId);
        tcatbal = new KsdsFile<>(TranCatBalRecord.CODEC, TranCatBalRecord::tranCatKey);
        transact = new KsdsFile<>(TransactionRecord.CODEC, TransactionRecord::tranId);
        xref.write(new CardXrefRecord(CARD, 1L, ACCT, ""));
        accounts.write(account("1000.00", "2025-12-31"));
        engine = new PostingEngine(xref, accounts, tcatbal, transact,
                Clock.fixed(Instant.parse("2026-01-02T03:04:05.678Z"), ZoneOffset.UTC));
    }

    @Test
    void postsValidCreditAndCreatesCategoryBalance() {
        assertThat(engine.process(tran("T1", CARD, "50.25", "2022-06-10"))).isEmpty();

        AccountRecord acct = accounts.read(ACCT).orElseThrow();
        assertThat(acct.acctCurrBal()).isEqualByComparingTo("50.25");
        assertThat(acct.acctCurrCycCredit()).isEqualByComparingTo("50.25");
        assertThat(acct.acctCurrCycDebit()).isEqualByComparingTo("0");
        assertThat(tcatbal.read(new TranCatKey(ACCT, "01", 1)).orElseThrow().tranCatBal())
                .isEqualByComparingTo("50.25");
        assertThat(transact.read("T1").orElseThrow().tranProcTs()).isEqualTo("2026-01-02-03.04.05.670000");
        assertThat(engine.sysout()).containsExactly("TCATBAL record not found for key : 00000000001010001.. Creating.");
        assertThat(engine.returnCode()).isZero();
    }

    @Test
    void debitGoesToCycleDebitAndAddsToExistingCategoryBalance() {
        tcatbal.write(new TranCatBalRecord(new TranCatKey(ACCT, "01", 1), new BigDecimal("10.00"), ""));

        assertThat(engine.process(tran("T1", CARD, "-4.50", "2022-06-10"))).isEmpty();

        AccountRecord acct = accounts.read(ACCT).orElseThrow();
        assertThat(acct.acctCurrCycDebit()).isEqualByComparingTo("-4.50");
        assertThat(acct.acctCurrCycCredit()).isEqualByComparingTo("0");
        assertThat(tcatbal.read(new TranCatKey(ACCT, "01", 1)).orElseThrow().tranCatBal())
                .isEqualByComparingTo("5.50");
        assertThat(engine.sysout()).isEmpty();
    }

    @Test
    void createdCategoryBalanceKeepsFillerOfLastRecordRead() {
        tcatbal.write(new TranCatBalRecord(new TranCatKey(ACCT, "01", 1), new BigDecimal("10.00"), "0".repeat(22)));

        engine.process(tran("T1", CARD, "1.00", "2022-06-10"));
        engine.process(tran("T2", CARD, "2.00", "2022-06-10", "02"));

        assertThat(tcatbal.read(new TranCatKey(ACCT, "02", 1)).orElseThrow().filler()).isEqualTo("0".repeat(22));
    }

    @Test
    void rejects100WhenCardNotInXref() {
        assertReject(tran("T1", "0000000000000000", "1.00", "2022-06-10"), 100, "INVALID CARD NUMBER FOUND");
    }

    @Test
    void rejects101WhenAccountMissing() {
        xref.write(new CardXrefRecord("1111222233334444", 2L, 99L, ""));
        assertReject(tran("T1", "1111222233334444", "1.00", "2022-06-10"), 101, "ACCOUNT RECORD NOT FOUND");
    }

    @Test
    void rejects102WhenOverLimit() {
        assertReject(tran("T1", CARD, "1000.01", "2022-06-10"), 102, "OVERLIMIT TRANSACTION");
    }

    @Test
    void acceptsAmountExactlyAtLimit() {
        assertThat(engine.process(tran("T1", CARD, "1000.00", "2022-06-10"))).isEmpty();
    }

    @Test
    void rejects103WhenAfterExpirationAndItOverridesOverLimit() {
        assertReject(tran("T1", CARD, "1.00", "2026-01-01"), 103, "TRANSACTION RECEIVED AFTER ACCT EXPIRATION");
        assertReject(tran("T2", CARD, "5000.00", "2026-01-01"), 103, "TRANSACTION RECEIVED AFTER ACCT EXPIRATION");
        assertThat(engine.process(tran("T3", CARD, "1.00", "2025-12-31"))).isEmpty();
    }

    @Test
    void rejectRecordIsDalytranPlusTrailer() {
        RejectRecord reject = engine.process(tran("T1", CARD, "1000.01", "2022-06-10")).orElseThrow();
        String line = reject.encode();
        assertThat(line).hasSize(430);
        assertThat(line.substring(0, 350)).isEqualTo(DailyTransactionRecord.CODEC.encode(reject.tran()));
        assertThat(line.substring(350)).isEqualTo(String.format("0102%-76s", "OVERLIMIT TRANSACTION"));
    }

    @Test
    void truncatesLikeCobolStoreWithoutSizeError() {
        assertThat(PostingEngine.truncate(new BigDecimal("1234567890.129"), 9, 2)).isEqualByComparingTo("234567890.12");
        assertThat(PostingEngine.truncate(new BigDecimal("-1234567890.12"), 9, 2)).isEqualByComparingTo("-234567890.12");
    }

    private void assertReject(DailyTransactionRecord tran, int reason, String description) {
        long before = engine.rejectCount();
        Optional<RejectRecord> reject = engine.process(tran);
        assertThat(reject).isPresent();
        assertThat(reject.get().reason()).isEqualTo(reason);
        assertThat(reject.get().description()).isEqualTo(description);
        assertThat(engine.rejectCount()).isEqualTo(before + 1);
        assertThat(engine.returnCode()).isEqualTo(4);
        assertThat(transact.read(tran.dalytranId())).isEmpty();
    }

    private static AccountRecord account(String limit, String expiration) {
        BigDecimal zero = new BigDecimal("0.00");
        return new AccountRecord(ACCT, "Y", zero, new BigDecimal(limit), zero, "2020-01-01", expiration,
                "2020-01-01", zero, zero, "", "", "");
    }

    private static DailyTransactionRecord tran(String id, String card, String amount, String origDate) {
        return tran(id, card, amount, origDate, "01");
    }

    private static DailyTransactionRecord tran(String id, String card, String amount, String origDate, String type) {
        return new DailyTransactionRecord(id, type, 1, "POS TERM", "Purchase", new BigDecimal(amount), 1L,
                "Merchant", "City", "12345", card, origDate + " 10:00:00.000000", "", "");
    }
}
