package com.carddemo.batch.intcalc;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.account.AccountRecord;
import com.carddemo.account.AccountStatus;
import com.carddemo.batch.BaselineRunProperties;
import com.carddemo.batch.harness.BufferedSink;
import com.carddemo.batch.harness.KeyedDataset;
import com.carddemo.batch.harness.KsdsInput;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.harness.Sysout;
import com.carddemo.card.CardXrefRecord;
import com.carddemo.common.AbendException;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.file.RecordFiles;
import com.carddemo.transaction.DisclosureGroupId;
import com.carddemo.transaction.DisclosureGroupRecord;
import com.carddemo.transaction.TranCatBalanceRecord;
import com.carddemo.transaction.TransactionRecord;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.core.JobParametersBuilder;

/**
 * CBACT04C rules on hand-built datasets: DEFAULT disclosure-group fallback, zero rate, negative balance, an account
 * without TCATBALF rows, account break and the unflushed last account, the transaction id scheme, truncation and the
 * abend paths. Interest values for negative/large balances were checked against GnuCOBOL 3.1.2 with the program's
 * PICs and COMPUTE.
 */
class Cbact04cTest {

    static final Clock GOLDEN = Clock.fixed(Instant.parse("2022-07-06T00:00:00Z"), ZoneOffset.UTC);
    static final String PARM = "2022071800";
    static final String TS = "2022-07-06-00.00.00.000000";

    @TempDir
    Path dir;

    final List<AccountRecord> accountRows = new ArrayList<>();
    final List<DisclosureGroupRecord> groupRows = new ArrayList<>();
    final List<CardXrefRecord> xrefRows = new ArrayList<>();
    KeyedDataset<Long, AccountRecord> accounts;
    BufferedSink systran;

    @BeforeEach
    void datasets() {
        for (long id = 1; id <= 3; id++) {
            accountRows.add(account(id, "1000.00", "GOLD"));
            xrefRows.add(new CardXrefRecord(String.format("41111111111111%02d", id), (int) id, id));
        }
        groupRows.add(group("DEFAULT", "01", 1, "15.00"));
        groupRows.add(group("DEFAULT", "03", 1, "0.00"));
        groupRows.add(group("GOLD", "02", 1, "24.00"));
        systran = new BufferedSink("TRANSACT");
    }

    static AccountRecord account(long id, String balance, String group) {
        return new AccountRecord(id, AccountStatus.ACTIVE, new BigDecimal(balance), new BigDecimal("5000.00"),
                new BigDecimal("1000.00"), "2010-01-01", "2030-01-01", "2015-01-01", new BigDecimal("300.00"),
                new BigDecimal("-120.00"), "12345", group);
    }

    static DisclosureGroupRecord group(String id, String type, int cat, String rate) {
        return new DisclosureGroupRecord(id, type, cat, new BigDecimal(rate));
    }

    static TranCatBalanceRecord bal(long acct, String type, int cat, String balance) {
        return new TranCatBalanceRecord(acct, type, cat, new BigDecimal(balance));
    }

    private Cbact04c.Result run(TranCatBalanceRecord... rows) throws IOException {
        List<FixedWidthRecord> images = new ArrayList<>();
        for (TranCatBalanceRecord r : rows) {
            images.add(TranCatBalanceRecord.MAPPER.toRecord(r, RecordEncoding.ASCII));
        }
        Path tcatbal = dir.resolve("TCATBALF");
        RecordFiles.writeLines("TCATBALF", tcatbal, images, false);
        accounts = KeyedDataset.memory("ACCTFILE", AccountRecord.MAPPER, AccountRecord::acctId,
                k -> String.format("%011d", k), accountRows);
        KeyedDataset<Long, CardXrefRecord> xref = KeyedDataset.memory("XREFFILE", CardXrefRecord.MAPPER,
                CardXrefRecord::acctId, k -> String.format("%011d", k), xrefRows);
        KeyedDataset<DisclosureGroupId, DisclosureGroupRecord> groups = KeyedDataset.memory("DISCGRP",
                DisclosureGroupRecord.MAPPER, r -> new DisclosureGroupId(r.acctGroupId(), r.tranTypeCd(),
                        r.tranCatCd()), IntcalcJobConfiguration::discgrpKey, groupRows);
        try (Sysout sysout = Sysout.open(dir.resolve("sysout.txt"))) {
            return new Cbact04c(KsdsInput.file("TCATBALF", tcatbal, TranCatBalanceRecord.MAPPER.layout(),
                    RecordEncoding.ASCII), xref, groups, accounts, systran, PARM, RecordEncoding.ASCII, sysout,
                    GOLDEN).run();
        }
    }

    private List<TransactionRecord> transactions() {
        return systran.records().stream().map(TransactionRecord.MAPPER::fromRecord).toList();
    }

    private AccountRecord acct(long id) {
        return accounts.contents().stream().filter(a -> a.acctId() == id).findFirst().orElseThrow();
    }

    private List<String> sysout() throws IOException {
        return Files.readAllLines(dir.resolve("sysout.txt"), StandardCharsets.ISO_8859_1).stream()
                .map(String::stripTrailing).toList();
    }

    @Test
    void aMissingGroupRowFallsBackToTheDefaultGroup() throws IOException {
        Cbact04c.Result result = run(bal(1, "01", 1, "1164.87"), bal(2, "01", 1, "0"));
        assertThat(result).isEqualTo(new Cbact04c.Result(2, 2, 1, ReturnCode.OK));
        assertThat(transactions().get(0).amount()).isEqualByComparingTo("14.56");
        assertThat(sysout()).containsExactly("START OF EXECUTION OF PROGRAM CBACT04C",
                "000000000010100010000011648G", "DISCLOSURE GROUP RECORD MISSING", "TRY WITH DEFAULT GROUP CODE",
                "000000000020100010000000000{", "DISCLOSURE GROUP RECORD MISSING", "TRY WITH DEFAULT GROUP CODE",
                "END OF EXECUTION OF PROGRAM CBACT04C");
    }

    @Test
    void theAccountsOwnGroupRowWinsOverDefault() throws IOException {
        groupRows.add(group("GOLD", "01", 1, "12.00"));
        run(bal(1, "01", 1, "1000.00"), bal(1, "02", 1, "100.00"), bal(2, "01", 1, "0"));
        assertThat(transactions()).extracting(TransactionRecord::amount)
                .usingElementComparator(BigDecimal::compareTo)
                .containsExactly(new BigDecimal("10.00"), new BigDecimal("2.00"), new BigDecimal("0.00"));
        assertThat(sysout()).doesNotContain("DISCLOSURE GROUP RECORD MISSING");
        assertThat(acct(1).currBal()).isEqualByComparingTo("1012.00");
    }

    @Test
    void aMissingDefaultGroupAbends() throws IOException {
        assertThatThrownBy(() -> run(bal(1, "05", 1, "10.00"))).isInstanceOf(AbendException.class);
        assertThat(sysout()).endsWith("DISCLOSURE GROUP RECORD MISSING", "TRY WITH DEFAULT GROUP CODE",
                "ERROR READING DEFAULT DISCLOSURE GROUP", "FILE STATUS IS: NNNN0023", "ABENDING PROGRAM");
    }

    @Test
    void aZeroRateWritesNoTransactionButTheAccountIsStillUpdatedAtTheBreak() throws IOException {
        Cbact04c.Result result = run(bal(1, "03", 1, "500.00"), bal(2, "01", 1, "0"));
        assertThat(result.written()).isEqualTo(1);
        assertThat(transactions()).extracting(TransactionRecord::description)
                .containsExactly("Int. for a/c 00000000002");
        AccountRecord one = acct(1);
        assertThat(one.currBal()).isEqualByComparingTo("1000.00");
        assertThat(one.currCycCredit()).isEqualByComparingTo("0");
        assertThat(one.currCycDebit()).isEqualByComparingTo("0");
    }

    @Test
    void aNegativeBalanceGivesNegativeInterestTruncatedTowardZero() throws IOException {
        run(bal(1, "01", 1, "-1164.87"), bal(2, "01", 1, "0"));
        assertThat(transactions().get(0).amount()).isEqualByComparingTo("-14.56");
        assertThat(acct(1).currBal()).isEqualByComparingTo("985.44");
        TransactionRecord negative = TransactionRecord.MAPPER.fromRecord(systran.records().get(0));
        assertThat(systran.records().get(0).text().substring(132, 143)).isEqualTo("0000000145O");
        assertThat(negative.amount()).isEqualByComparingTo("-14.56");
    }

    @Test
    void anAccountWithoutTcatbalfRowsIsNotTouched() throws IOException {
        Cbact04c.Result result = run(bal(1, "01", 1, "100.00"), bal(3, "01", 1, "100.00"));
        assertThat(result.accountsUpdated()).isEqualTo(1);
        assertThat(acct(2)).isEqualTo(account(2, "1000.00", "GOLD"));
        assertThat(transactions()).extracting(TransactionRecord::description)
                .containsExactly("Int. for a/c 00000000001", "Int. for a/c 00000000003");
    }

    @Test
    void interestAccumulatesPerAccountAndTheLastAccountIsNeverRewritten() throws IOException {
        run(bal(1, "01", 1, "1000.00"), bal(1, "02", 1, "500.00"), bal(1, "03", 1, "900.00"),
                bal(2, "01", 1, "200.00"), bal(2, "02", 1, "-100.00"));
        AccountRecord one = acct(1);
        assertThat(one.currBal()).isEqualByComparingTo("1022.50");
        assertThat(one.currCycCredit()).isEqualByComparingTo("0");
        assertThat(one.currCycDebit()).isEqualByComparingTo("0");
        // End of file leaves the loop before 1050-UPDATE-ACCOUNT runs for the last account (as in the baseline).
        assertThat(acct(2)).isEqualTo(account(2, "1000.00", "GOLD"));
        assertThat(transactions()).extracting(TransactionRecord::amount).usingElementComparator(BigDecimal::compareTo)
                .containsExactly(new BigDecimal("12.50"), new BigDecimal("10.00"), new BigDecimal("2.50"),
                        new BigDecimal("-2.00"));
    }

    @Test
    void transactionsCarryTheParmBasedIdAndTheFixedFields() throws IOException {
        run(bal(1, "01", 1, "1000.00"), bal(2, "01", 1, "200.00"), bal(2, "02", 1, "0.49"));
        assertThat(transactions()).containsExactly(
                new TransactionRecord("2022071800000001", "01", 5, "System", "Int. for a/c 00000000001",
                        new BigDecimal("12.50"), 0, "", "", "", "4111111111111101", TS, TS),
                new TransactionRecord("2022071800000002", "01", 5, "System", "Int. for a/c 00000000002",
                        new BigDecimal("2.50"), 0, "", "", "", "4111111111111102", TS, TS),
                new TransactionRecord("2022071800000003", "01", 5, "System", "Int. for a/c 00000000002",
                        new BigDecimal("0.00"), 0, "", "", "", "4111111111111102", TS, TS));
        assertThat(systran.records()).allSatisfy(r -> assertThat(r.length()).isEqualTo(Cbact04c.TRANSACT_LRECL));
    }

    @Test
    void aMissingAccountOrCrossReferenceAbends() throws IOException {
        assertThatThrownBy(() -> run(bal(9, "01", 1, "1.00"))).isInstanceOf(AbendException.class);
        assertThat(sysout()).endsWith("ACCOUNT NOT FOUND: 00000000009", "ERROR READING ACCOUNT FILE",
                "FILE STATUS IS: NNNN0023", "ABENDING PROGRAM");

        xrefRows.removeIf(x -> x.acctId() == 2);
        systran = new BufferedSink("TRANSACT");
        assertThatThrownBy(() -> run(bal(1, "01", 1, "1000.00"), bal(2, "01", 1, "1.00")))
                .isInstanceOf(AbendException.class);
        assertThat(sysout()).endsWith("ACCOUNT NOT FOUND: 00000000002", "ERROR READING XREF FILE",
                "FILE STATUS IS: NNNN0023", "ABENDING PROGRAM");
        // The rewrite of account 1 at the break was issued before the abend and stays (VSAM semantics).
        assertThat(acct(1).currBal()).isEqualByComparingTo("1012.50");
    }

    @Test
    void monthlyInterestTruncatesIntoS9_9V99() {
        assertThat(Cbact04c.monthlyInterest(new BigDecimal("1164.87"), new BigDecimal("15.00"))).isEqualTo("14.56");
        assertThat(Cbact04c.monthlyInterest(new BigDecimal("-1164.87"), new BigDecimal("15.00")))
                .isEqualTo("-14.56");
        assertThat(Cbact04c.monthlyInterest(new BigDecimal("0.07"), new BigDecimal("15.00"))).isEqualTo("0.00");
        assertThat(Cbact04c.monthlyInterest(new BigDecimal("-0.07"), new BigDecimal("15.00"))).isEqualTo("0.00");
        assertThat(Cbact04c.monthlyInterest(new BigDecimal("123.45"), new BigDecimal("12.34"))).isEqualTo("1.26");
        assertThat(Cbact04c.monthlyInterest(new BigDecimal("-123.45"), new BigDecimal("12.34"))).isEqualTo("-1.26");
        // 8333324999.9166... loses its high-order digit in S9(09)V99, as under GnuCOBOL (no ON SIZE ERROR).
        assertThat(Cbact04c.monthlyInterest(new BigDecimal("999999999.99"), new BigDecimal("9999.99")))
                .isEqualTo("333324999.91");
    }

    @Test
    void transactionIdIsTheTenCharacterParmPlusASixDigitSuffix() {
        assertThat(Cbact04c.tranId("2022071800", 1)).isEqualTo("2022071800000001");
        assertThat(Cbact04c.tranId("20220718", 42)).isEqualTo("20220718  000042");
        assertThat(Cbact04c.tranId("202207180099", 7)).isEqualTo("2022071800000007");
    }

    @Test
    void parmComesFromTheJobParameterThenTheBaselinePinThenTheRunDate() {
        BaselineRunProperties pinned = new BaselineRunProperties("2022071800", null, null, null);
        BaselineRunProperties none = new BaselineRunProperties(null, null, null, null);
        assertThat(IntcalcJobConfiguration.parm(new JobParametersBuilder().addString("PARM", "2023010100")
                .toJobParameters(), pinned)).isEqualTo("2023010100");
        assertThat(IntcalcJobConfiguration.parm(new JobParametersBuilder().toJobParameters(), pinned))
                .isEqualTo("2022071800");
        assertThat(IntcalcJobConfiguration.parm(new JobParametersBuilder()
                .addLocalDate("run-date", LocalDate.of(2024, 2, 29)).toJobParameters(), none))
                .isEqualTo("2024022900");
    }
}
