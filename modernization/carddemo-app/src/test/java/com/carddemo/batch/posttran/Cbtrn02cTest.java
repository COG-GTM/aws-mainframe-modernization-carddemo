package com.carddemo.batch.posttran;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.account.AccountRecord;
import com.carddemo.account.AccountStatus;
import com.carddemo.batch.harness.KsdsInput;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.harness.Sysout;
import com.carddemo.card.CardXrefRecord;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.file.RecordFiles;
import com.carddemo.transaction.DailyTransactionRecord;
import com.carddemo.transaction.TranCatBalanceId;
import com.carddemo.transaction.TranCatBalanceRecord;
import com.carddemo.transaction.TransactionRecord;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/** CBTRN02C rules on hand-built datasets: every reject path, boundaries, sign handling and TCATBALF creation. */
class Cbtrn02cTest {

    static final String CARD = "4111111111111111";
    static final String ORPHAN_CARD = "4222222222222222";
    static final long ACCT = 1L;
    static final Clock GOLDEN = Clock.fixed(Instant.parse("2022-07-06T00:00:00Z"), ZoneOffset.UTC);

    @TempDir
    Path dir;

    KeyedDataset<String, CardXrefRecord> xref;
    KeyedDataset<Long, AccountRecord> accounts;
    KeyedDataset<TranCatBalanceId, TranCatBalanceRecord> balances;
    KeyedDataset<String, TransactionRecord> transactions;
    BufferedSink rejects;
    final AtomicInteger units = new AtomicInteger();

    @BeforeEach
    void datasets() {
        xref = KeyedDataset.memory("XREFFILE", CardXrefRecord.MAPPER, CardXrefRecord::cardNum, k -> k,
                List.of(new CardXrefRecord(CARD, 7, ACCT), new CardXrefRecord(ORPHAN_CARD, 8, 99L)));
        accounts(account("2023-01-01", "100.00", "50.00", "10.00"));
        balances = KeyedDataset.memory("TCATBALF", TranCatBalanceRecord.MAPPER,
                r -> new TranCatBalanceId(r.acctId(), r.tranTypeCd(), r.tranCatCd()), Cbtrn02c::tcatKey,
                List.of(new TranCatBalanceRecord(ACCT, "01", 1, new BigDecimal("5.00"))));
        transactions = KeyedDataset.memory("TRANFILE", TransactionRecord.MAPPER, TransactionRecord::tranId, k -> k,
                List.of());
        rejects = new BufferedSink("DALYREJS");
    }

    private void accounts(AccountRecord account) {
        accounts = KeyedDataset.memory("ACCTFILE", AccountRecord.MAPPER, AccountRecord::acctId,
                k -> String.format("%011d", k), List.of(account));
    }

    static AccountRecord account(String expiration, String limit, String cycCredit, String cycDebit) {
        return new AccountRecord(ACCT, AccountStatus.ACTIVE, new BigDecimal("40.00"), new BigDecimal(limit),
                new BigDecimal("20.00"), "2010-01-01", expiration, "2015-01-01", new BigDecimal(cycCredit),
                new BigDecimal(cycDebit), "12345", "DEFAULT");
    }

    static DailyTransactionRecord tran(String id, String card, String amount, String origTs, String type, int cat) {
        return new DailyTransactionRecord(id, type, cat, "POS TERM", "Purchase", new BigDecimal(amount), 800000001,
                "Shop", "City", "12345", card, origTs, "");
    }

    static DailyTransactionRecord tran(String id, String card, String amount) {
        return tran(id, card, amount, "2022-06-10 19:27:53.000000", "01", 1);
    }

    private Cbtrn02c.Result run(DailyTransactionRecord... trans) throws IOException {
        return run(GOLDEN, trans);
    }

    private Cbtrn02c.Result run(Clock clock, DailyTransactionRecord... trans) throws IOException {
        List<FixedWidthRecord> images = new ArrayList<>();
        for (DailyTransactionRecord t : trans) {
            images.add(DailyTransactionRecord.MAPPER.toRecord(t, RecordEncoding.ASCII));
        }
        Path dalytran = dir.resolve("DALYTRAN");
        RecordFiles.writeLines("DALYTRAN", dalytran, images, false);
        try (Sysout sysout = Sysout.open(dir.resolve("sysout.txt"))) {
            return new Cbtrn02c(KsdsInput.file("DALYTRAN", dalytran, DailyTransactionRecord.MAPPER.layout(),
                    RecordEncoding.ASCII), transactions, xref, rejects, accounts, balances, sysout, clock,
                    work -> {
                        units.incrementAndGet();
                        work.run();
                    }).run();
        }
    }

    private List<String> trailers() {
        return rejects.records().stream().map(r -> r.text().substring(350)).toList();
    }

    private AccountRecord acct() {
        return accounts.contents().get(0);
    }

    @Test
    void cardNotInXrefIsRejectedWith100() throws IOException {
        Cbtrn02c.Result result = run(tran("T1", "4999999999999999", "1.00"));
        assertThat(result.rejected()).isEqualTo(1);
        assertThat(result.returnCode()).isEqualTo(ReturnCode.WARNING);
        assertThat(trailers()).containsExactly(String.format("0100%-76s", "INVALID CARD NUMBER FOUND"));
        assertThat(rejects.records().get(0).bytes()).hasSize(Cbtrn02c.REJECT_LRECL);
        assertThat(transactions.contents()).isEmpty();
        assertThat(acct()).isEqualTo(account("2023-01-01", "100.00", "50.00", "10.00"));
    }

    @Test
    void accountNotFoundIsRejectedWith101() throws IOException {
        run(tran("T1", ORPHAN_CARD, "1.00"));
        assertThat(trailers()).containsExactly(String.format("0101%-76s", "ACCOUNT RECORD NOT FOUND"));
    }

    @Test
    void creditLimitExceededIsRejectedWith102() throws IOException {
        // temp balance = 50.00 - 10.00 + 70.01 = 110.01 > 100.00
        run(tran("T1", CARD, "70.01"));
        assertThat(trailers()).containsExactly(String.format("0102%-76s", "OVERLIMIT TRANSACTION"));
        assertThat(acct().currBal()).isEqualByComparingTo("40.00");
        assertThat(balances.contents()).containsExactly(new TranCatBalanceRecord(ACCT, "01", 1,
                new BigDecimal("5.00")));
    }

    @Test
    void creditLimitExactlyReachedIsPosted() throws IOException {
        // temp balance = 50.00 - 10.00 + 60.00 = 100.00 = limit
        Cbtrn02c.Result result = run(tran("T1", CARD, "60.00"));
        assertThat(result.rejected()).isZero();
        assertThat(result.returnCode()).isEqualTo(ReturnCode.OK);
        assertThat(acct().currBal()).isEqualByComparingTo("100.00");
        assertThat(acct().currCycCredit()).isEqualByComparingTo("110.00");
    }

    @Test
    void transactionAfterExpirationIsRejectedWith103() throws IOException {
        accounts(account("2022-06-09", "100.00", "50.00", "10.00"));
        run(tran("T1", CARD, "1.00"));
        assertThat(trailers()).containsExactly(
                String.format("0103%-76s", "TRANSACTION RECEIVED AFTER ACCT EXPIRATION"));
    }

    @Test
    void cardExpiringOnTheTransactionDateIsPosted() throws IOException {
        accounts(account("2022-06-10", "100.00", "50.00", "10.00"));
        Cbtrn02c.Result result = run(tran("T1", CARD, "1.00"));
        assertThat(result.rejected()).isZero();
        assertThat(transactions.contents()).hasSize(1);
    }

    @Test
    void expirationIsCheckedAfterTheLimitAndOverwritesTheReason() throws IOException {
        accounts(account("2022-06-09", "100.00", "50.00", "10.00"));
        run(tran("T1", CARD, "999.00"));
        assertThat(trailers()).containsExactly(
                String.format("0103%-76s", "TRANSACTION RECEIVED AFTER ACCT EXPIRATION"));
    }

    @Test
    void negativeAmountIsAddedToTheDebitTotal() throws IOException {
        run(tran("T1", CARD, "-25.50"));
        AccountRecord a = acct();
        assertThat(a.currBal()).isEqualByComparingTo("14.50");
        assertThat(a.currCycCredit()).isEqualByComparingTo("50.00");
        assertThat(a.currCycDebit()).isEqualByComparingTo("-15.50");
        assertThat(balances.contents()).containsExactly(new TranCatBalanceRecord(ACCT, "01", 1,
                new BigDecimal("-20.50")));
    }

    @Test
    void zeroAmountCountsAsCredit() throws IOException {
        run(tran("T1", CARD, "0.00"));
        assertThat(acct().currCycCredit()).isEqualByComparingTo("50.00");
        assertThat(acct().currCycDebit()).isEqualByComparingTo("10.00");
        assertThat(transactions.contents()).hasSize(1);
    }

    @Test
    void missingCategoryBalanceRowIsCreated() throws IOException {
        run(tran("T1", CARD, "12.34", "2022-06-10 19:27:53.000000", "02", 5),
                tran("T2", CARD, "1.66", "2022-06-10 19:27:53.000000", "02", 5));
        assertThat(balances.contents()).containsExactly(
                new TranCatBalanceRecord(ACCT, "01", 1, new BigDecimal("5.00")),
                new TranCatBalanceRecord(ACCT, "02", 5, new BigDecimal("14.00")));
        assertThat(Files.readAllLines(dir.resolve("sysout.txt"), StandardCharsets.ISO_8859_1))
                .containsOnlyOnce("TCATBAL record not found for key : 00000000001020005.. Creating.");
    }

    @Test
    void postedTransactionCopiesTheDailyRecordAndStampsTheProcessingTime() throws IOException {
        Clock clock = Clock.fixed(Instant.parse("2022-07-06T13:45:07.987Z"), ZoneOffset.UTC);
        DailyTransactionRecord t = tran("0000000001774260", CARD, "1.00");
        run(clock, t);
        assertThat(transactions.contents()).containsExactly(new TransactionRecord(t.tranId(), t.tranTypeCd(),
                t.tranCatCd(), t.source(), t.description(), t.amount(), t.merchantId(), t.merchantName(),
                t.merchantCity(), t.merchantZip(), t.cardNum(), t.origTs(), "2022-07-06-13.45.07.980000"));
    }

    @Test
    void eachRecordIsOneUnitOfWorkAndRejectsKeepTheOriginalBytes() throws IOException {
        DailyTransactionRecord bad = tran("T2", "4999999999999999", "3.00");
        Cbtrn02c.Result result = run(tran("T1", CARD, "1.00"), bad, tran("T3", CARD, "2.00"));
        assertThat(units).hasValue(3);
        assertThat(result).isEqualTo(new Cbtrn02c.Result(3, 1, ReturnCode.WARNING));
        assertThat(result.posted()).isEqualTo(2);
        assertThat(rejects.records().get(0).text().substring(0, 350))
                .isEqualTo(DailyTransactionRecord.MAPPER.toRecord(bad, RecordEncoding.ASCII).text());
        List<String> sysout = Files.readAllLines(dir.resolve("sysout.txt"), StandardCharsets.ISO_8859_1);
        assertThat(sysout).contains("TRANSACTIONS PROCESSED :000000003", "TRANSACTIONS REJECTED  :000000001");
    }

    @Test
    void temporaryBalanceTruncatesLikeTheReceivingPicture() {
        // WS-TEMP-BAL is S9(09)V99: 1,000,000,000.00 loses its high-order digit and compares as 0.00
        AccountRecord a = new AccountRecord(ACCT, AccountStatus.ACTIVE, BigDecimal.ZERO, new BigDecimal("1.00"),
                BigDecimal.ZERO, "2010-01-01", "2030-01-01", "2015-01-01", new BigDecimal("999999999.99"),
                BigDecimal.ZERO, "", "");
        assertThat(Cbtrn02c.validate(tran("T1", CARD, "0.01"), a).valid()).isTrue();
        assertThat(Cbtrn02c.validate(tran("T1", CARD, "1.02"), a).reason()).isEqualTo(Cbtrn02c.OVERLIMIT);
    }
}
